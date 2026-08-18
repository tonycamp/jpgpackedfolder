SuperStrict

Import BRL.StandardIO
Import BRL.Pixmap
Import BRL.JPGLoader
Import BRL.PNGLoader
Import BRL.Random

Graphics 1920,1080


' ============================================================
' DATASET CODEBOOK ENCODER
'
' 8x8
' RGB -> YUV -> /2
' SEM HAAR
' SEM ZIGZAG
'
' TREINO:
'   10 ITERACOES REAIS
'
' DATASET:
'   codeY U16
'   codeU U16
'   codeV U16
'   Y residual 64 bytes
'   U residual 4 bytes
'   V residual 4 bytes
'
' TOTAL BLOCO = 78 BYTES
' ============================================================


Const BLOCKSIZE:Int = 8
Const BLOCKPIXELS:Int = 64

Const Y_RESIDUAL_COUNT:Int = 64
Const C_RESIDUAL_COUNT:Int = 4

Const Y_RESIDUAL_BYTES:Int = 64
Const C_RESIDUAL_BYTES:Int = 4

Const BLOCK_DATA_BYTES:Int = 78

Const CODEBOOKSIZEY:Int = 4096
Const CODEBOOKSIZEC:Int = 1024

Const HASHCAPACITY:Int = 131072
Const INDEXSIZE:Int = 256

' ============================================================
' NUMERO DE ITERACOES DO TREINO
' ============================================================

Const TRAIN_ITERATIONS:Int = 8

' ============================================================
' DATASET VERSION
' ============================================================

Const HEADER_VERSION:Int = 6


' ============================================================
' IMAGE INFO
' ============================================================

Type TImageInfo

	Field name:String
	Field path:String

	Field width:Int
	Field height:Int

	Field blocks:Int

End Type


' ============================================================
' CODEBOOK
' ============================================================

Type TCodeBook

	Field size:Int

	Field data:Int[]

	Field bucketHead:Int[]
	Field bucketNext:Int[]

	Field hashKey:Int[]
	Field hashCode:Int[]
	Field hashUsed:Byte[]

	Field hashCapacity:Int

End Type


' ============================================================
' CREATE CODEBOOK
' ============================================================

Function CreateCodeBook:TCodeBook(codeCount:Int)

	Local book:TCodeBook

	book = New TCodeBook

	book.size = codeCount

	book.data = New Int[codeCount * BLOCKPIXELS]

	book.bucketHead = New Int[INDEXSIZE]
	book.bucketNext = New Int[codeCount]


	For Local i:Int = 0 Until INDEXSIZE

		book.bucketHead[i] = -1

	Next


	For Local i:Int = 0 Until codeCount

		book.bucketNext[i] = -1

	Next


	book.hashCapacity = HASHCAPACITY

	book.hashKey = New Int[book.hashCapacity]
	book.hashCode = New Int[book.hashCapacity]
	book.hashUsed = New Byte[book.hashCapacity]


	For Local i:Int = 0 Until book.hashCapacity

		book.hashKey[i] = 0
		book.hashCode[i] = -1
		book.hashUsed[i] = 0

	Next


	Return book

End Function


' ============================================================
' CLAMP
' ============================================================

Function ClampByte:Int(value:Int)

	If value < 0 Then

		Return 0

	EndIf


	If value > 255 Then

		Return 255

	EndIf


	Return value

End Function


' ============================================================
' JOIN PATH
' ============================================================

Function JoinPath:String(folder:String,name:String)

	If folder.EndsWith("/") Then

		Return folder + name

	EndIf


	If folder.EndsWith("\") Then

		Return folder + name

	EndIf


	Return folder + "/" + name

End Function


' ============================================================
' IMAGE EXTENSION
' ============================================================

Function IsImageFile:Int(name:String)

	Local n:String

	n = name.ToLower()


	If n.EndsWith(".png") Then Return True
	If n.EndsWith(".jpg") Then Return True
	If n.EndsWith(".jpeg") Then Return True
	If n.EndsWith(".bmp") Then Return True
	If n.EndsWith(".tga") Then Return True


	Return False

End Function


' ============================================================
' GET IMAGE FILES
' ============================================================

Function GetImageFiles:String[](folder:String)

	Local result:String[]
	Local all:String[]

	result = New String[0]

	all = LoadDir(folder)


	If all = Null Then

		Return result

	EndIf


	For Local name:String = EachIn all

		If name = "." Then Continue
		If name = ".." Then Continue


		If IsImageFile(name) Then

			Local oldLength:Int

			oldLength = result.Length

			result = result[..oldLength + 1]

			result[oldLength] = name

		EndIf

	Next


	Return result

End Function


' ============================================================
' RGB -> Y
' ============================================================

Function RGBToY:Int(r:Int,g:Int,b:Int)

	Local y:Int

	y = Int(0.257 * Float(r) + 0.504 * Float(g) + 0.098 * Float(b) + 16.0)

	Return ClampByte(y)

End Function


' ============================================================
' RGB -> U
' ============================================================

Function RGBToU:Int(r:Int,g:Int,b:Int)

	Local u:Int

	u = Int(-0.148 * Float(r) - 0.291 * Float(g) + 0.439 * Float(b) + 128.0)

	Return ClampByte(u)

End Function


' ============================================================
' RGB -> V
' ============================================================

Function RGBToV:Int(r:Int,g:Int,b:Int)

	Local v:Int

	v = Int(0.439 * Float(r) - 0.368 * Float(g) - 0.071 * Float(b) + 128.0)

	Return ClampByte(v)

End Function


' ============================================================
' YUV -> RGB
' ============================================================

Function YUVToR:Int(y:Int,u:Int,v:Int)

	Local yy:Float
	Local vv:Float

	yy = 1.164 * Float(y - 16)

	vv = Float(v - 128)

	Return ClampByte(Int(yy + 1.596 * vv))

End Function


Function YUVToG:Int(y:Int,u:Int,v:Int)

	Local yy:Float
	Local uu:Float
	Local vv:Float

	yy = 1.164 * Float(y - 16)

	uu = Float(u - 128)
	vv = Float(v - 128)

	Return ClampByte(Int(yy - 0.392 * uu - 0.813 * vv))

End Function


Function YUVToB:Int(y:Int,u:Int,v:Int)

	Local yy:Float
	Local uu:Float

	yy = 1.164 * Float(y - 16)

	uu = Float(u - 128)

	Return ClampByte(Int(yy + 2.017 * uu))

End Function


' ============================================================
' YUV / 2
' ============================================================

Function YUVHalf:Int(value:Int)

	Return value / 2

End Function


' ============================================================
' GET PIXEL CHANNEL
' ============================================================

Function GetPixelChannel:Int(pix:TPixmap,x:Int,y:Int,channel:Int)

	Local pixel:Int

	pixel = ReadPixel(pix,x,y)


	Local r:Int
	Local g:Int
	Local b:Int

	r = (pixel Shr 16) & $FF
	g = (pixel Shr 8) & $FF
	b = pixel & $FF


	Local value:Int


	If channel = 0 Then

		value = RGBToY(r,g,b)

	ElseIf channel = 1 Then

		value = RGBToU(r,g,b)

	Else

		value = RGBToV(r,g,b)

	EndIf


	value = YUVHalf(value)


	Return value

End Function


' ============================================================
' BUILD IMAGE LIST
' ============================================================

Function BuildImageList:TImageInfo[](folder:String,files:String[])

	Local result:TImageInfo[]

	result = New TImageInfo[0]


	For Local name:String = EachIn files

		Local path:String

		path = JoinPath(folder,name)


		Local pix:TPixmap

		pix = LoadPixmap(path)


		If pix = Null Then

			Print "AVISO: nao consegui abrir " + path

			Continue

		EndIf


		If pix.width <= 0 Or pix.height <= 0 Then

			Print "IGNORADA: dimensao invalida: " + name

			Continue

		EndIf


		If pix.width Mod BLOCKSIZE <> 0 Then

			Print "IGNORADA: largura nao multipla de 8: " + name

			Continue

		EndIf


		If pix.height Mod BLOCKSIZE <> 0 Then

			Print "IGNORADA: altura nao multipla de 8: " + name

			Continue

		EndIf


		Local info:TImageInfo

		info = New TImageInfo

		info.name = name
		info.path = path

		info.width = pix.width
		info.height = pix.height

		info.blocks = (pix.width / BLOCKSIZE) * (pix.height / BLOCKSIZE)


		Local oldLength:Int

		oldLength = result.Length

		result = result[..oldLength + 1]

		result[oldLength] = info


		Print "Imagem: " + name
		Print "  Size: " + pix.width + " x " + pix.height
		Print "  Blocos: " + info.blocks

	Next


	Return result

End Function


' ============================================================
' COUNT TOTAL BLOCKS
' ============================================================

Function CountTotalBlocks:Long(images:TImageInfo[])

	Local total:Long

	total = 0


	For Local info:TImageInfo = EachIn images

		If info <> Null Then

			total :+ info.blocks

		EndIf

	Next


	Return total

End Function


' ============================================================
' COPY BLOCK TO CODEBOOK
' ============================================================

Function CopyBlockToBook(book:TCodeBook,code:Int,pix:TPixmap,bx:Int,by:Int,channel:Int)

	If code < 0 Or code >= book.size Then

		Return

	EndIf


	Local base:Int

	base = code * BLOCKPIXELS


	For Local yy:Int = 0 Until BLOCKSIZE

		For Local xx:Int = 0 Until BLOCKSIZE

			Local px:Int
			Local py:Int
			Local p:Int

			px = bx * BLOCKSIZE + xx
			py = by * BLOCKSIZE + yy

			p = yy * BLOCKSIZE + xx


			book.data[base + p] = GetPixelChannel(pix,px,py,channel)

		Next

	Next

End Function


' ============================================================
' COPY CODE
' ============================================================

Function CopyCode(book:TCodeBook,destination:Int,source:Int)

	If destination < 0 Or destination >= book.size Then Return
	If source < 0 Or source >= book.size Then Return


	Local dstBase:Int
	Local srcBase:Int

	dstBase = destination * BLOCKPIXELS
	srcBase = source * BLOCKPIXELS


	For Local p:Int = 0 Until BLOCKPIXELS

		book.data[dstBase + p] = book.data[srcBase + p]

	Next

End Function


' ============================================================
' HASH BOOK BLOCK
' ============================================================

Function HashBookBlock:Int(book:TCodeBook,code:Int)

	Local h:Long
	Local base:Int

	h = 2166136261

	base = code * BLOCKPIXELS


	For Local i:Int = 0 Until BLOCKPIXELS

		h = h + Long(book.data[base + i] + 1) * Long(i + 1) * 16777619

		h = h * 16777619

		h = h & $FFFFFFFF

	Next


	Return Int(h)

End Function


' ============================================================
' HASH BLOCK
' ============================================================

Function HashBlock:Int(block:Int[])

	Local h:Long

	h = 2166136261


	For Local i:Int = 0 Until BLOCKPIXELS

		h = h + Long(block[i] + 1) * Long(i + 1) * 16777619

		h = h * 16777619

		h = h & $FFFFFFFF

	Next


	Return Int(h)

End Function


' ============================================================
' HASH SLOT
' ============================================================

Function HashSlot:Int(hash:Int,capacity:Int)

	If capacity <= 0 Then

		Return 0

	EndIf


	Return hash & (capacity - 1)

End Function


' ============================================================
' CLEAR HASH
' ============================================================

Function ClearHashTable(book:TCodeBook)

	For Local i:Int = 0 Until book.hashCapacity

		book.hashUsed[i] = 0
		book.hashKey[i] = 0
		book.hashCode[i] = -1

	Next

End Function


' ============================================================
' HASH INSERT
' ============================================================

Function HashInsert(book:TCodeBook,hash:Int,code:Int)

	Local slot:Int

	slot = HashSlot(hash,book.hashCapacity)


	For Local attempt:Int = 0 Until book.hashCapacity

		If book.hashUsed[slot] = 0 Then

			book.hashUsed[slot] = 1

			book.hashKey[slot] = hash

			book.hashCode[slot] = code

			Return

		EndIf


		If book.hashKey[slot] = hash Then

			Return

		EndIf


		slot :+ 1


		If slot >= book.hashCapacity Then

			slot = 0

		EndIf

	Next

End Function


' ============================================================
' HASH FIND
' ============================================================

Function HashFind:Int(book:TCodeBook,hash:Int)

	Local slot:Int

	slot = HashSlot(hash,book.hashCapacity)


	For Local attempt:Int = 0 Until book.hashCapacity

		If book.hashUsed[slot] = 0 Then

			Return -1

		EndIf


		If book.hashKey[slot] = hash Then

			Return book.hashCode[slot]

		EndIf


		slot :+ 1


		If slot >= book.hashCapacity Then

			slot = 0

		EndIf

	Next


	Return -1

End Function


' ============================================================
' CALCULATE AVERAGE
' ============================================================

Function CalculateBlockAverage:Int(book:TCodeBook,code:Int)

	Local base:Int
	Local total:Int

	base = code * BLOCKPIXELS

	total = 0


	For Local p:Int = 0 Until BLOCKPIXELS

		total :+ book.data[base + p]

	Next


	Return total / BLOCKPIXELS

End Function


' ============================================================
' INDEX
' ============================================================

Function MakeIndex:Int(avg:Int)

	If avg < 0 Then avg = 0

	If avg > 255 Then avg = 255


	Return avg

End Function


' ============================================================
' BUILD CODEBOOK INDEX
' ============================================================

Function BuildCodeBookIndex(book:TCodeBook)

	Print "A construir indice..."


	ClearHashTable(book)


	For Local i:Int = 0 Until INDEXSIZE

		book.bucketHead[i] = -1

	Next


	For Local i:Int = 0 Until book.size

		book.bucketNext[i] = -1

	Next


	For Local code:Int = 0 Until book.size

		Local avg:Int

		avg = CalculateBlockAverage(book,code)


		Local bucket:Int

		bucket = MakeIndex(avg)


		book.bucketNext[code] = book.bucketHead[bucket]

		book.bucketHead[bucket] = code


		Local hash:Int

		hash = HashBookBlock(book,code)


		If HashFind(book,hash) = -1 Then

			HashInsert(book,hash,code)

		EndIf

	Next


	Print "Indice pronto."

End Function


' ============================================================
' BLOCK DISTANCE
' ============================================================

Function BlockDistance:Int(book:TCodeBook,code:Int,block:Int[])

	Local base:Int
	Local distance:Int

	base = code * BLOCKPIXELS

	distance = 0


	For Local p:Int = 0 Until BLOCKPIXELS

		Local d:Int

		d = book.data[base + p] - block[p]

		distance :+ d * d

	Next


	Return distance

End Function


' ============================================================
' FIND NEAREST
' ============================================================

Function FindNearest:Int(book:TCodeBook,block:Int[])

	Local hash:Int

	hash = HashBlock(block)


	Local exactCode:Int

	exactCode = HashFind(book,hash)


	If exactCode >= 0 Then

		Local base:Int
		Local exact:Int

		base = exactCode * BLOCKPIXELS
		exact = True


		For Local p:Int = 0 Until BLOCKPIXELS

			If book.data[base + p] <> block[p] Then

				exact = False

				Exit

			EndIf

		Next


		If exact Then Return exactCode

	EndIf


	Local total:Int

	total = 0


	For Local p:Int = 0 Until BLOCKPIXELS

		total :+ block[p]

	Next


	Local average:Int

	average = total / BLOCKPIXELS


	Local bestCode:Int
	Local bestDistance:Int
	Local found:Int

	bestCode = 0

	bestDistance = 2147483647

	found = False


	For Local delta:Int = -16 To 16

		Local bucket:Int

		bucket = average + delta


		If bucket < 0 Then Continue

		If bucket >= INDEXSIZE Then Continue


		Local code:Int

		code = book.bucketHead[bucket]


		While code >= 0

			Local distance:Int

			distance = BlockDistance(book,code,block)


			If distance < bestDistance Then

				bestDistance = distance

				bestCode = code

				found = True

			EndIf


			code = book.bucketNext[code]

		Wend

	Next


	If found Then Return bestCode


	For Local code:Int = 0 Until book.size

		Local distance:Int

		distance = BlockDistance(book,code,block)


		If distance < bestDistance Then

			bestDistance = distance

			bestCode = code

		EndIf

	Next


	Return bestCode

End Function


' ============================================================
' TRAIN CODEBOOK
'
' 10 ITERACOES REAIS
'
' 1. Inicializa codebook
' 2. Cada bloco encontra o code mais proximo
' 3. Acumula os pixels
' 4. Calcula a media
' 5. Atualiza codebook
' 6. Reconstrói indice
' 7. Repete 10 vezes
' ============================================================

Function TrainCodeBook:TCodeBook(images:TImageInfo[],channel:Int,codeCount:Int)

	Print ""
	Print "=========================================="


	If channel = 0 Then

		Print "TREINO LUMA Y / 2"

	ElseIf channel = 1 Then

		Print "TREINO CHROMA U / 2"

	Else

		Print "TREINO CHROMA V / 2"

	EndIf


	Print "CODEBOOK = " + codeCount
	Print "ITERACOES = " + TRAIN_ITERATIONS

	Print "=========================================="


	Local totalBlocks:Long

	totalBlocks = CountTotalBlocks(images)


	Local book:TCodeBook

	book = CreateCodeBook(codeCount)


	If totalBlocks <= 0 Then

		Print "ERRO: dataset sem blocos."

		Return book

	EndIf


	' ========================================================
	' INICIALIZACAO
	' ========================================================

	Print ""
	Print "Inicializar codebook..."


	Local nextCode:Int
	Local globalBlock:Long

	nextCode = 0
	globalBlock = 0


	For Local info:TImageInfo = EachIn images

		If nextCode >= codeCount Then Exit


		Print "Inicializacao: " + info.name


		Local pix:TPixmap

		pix = LoadPixmap(info.path)


		If pix = Null Then

			Print "AVISO: nao consegui carregar " + info.path

			Continue

		EndIf


		Local blocksX:Int
		Local blocksY:Int

		blocksX = pix.width / BLOCKSIZE
		blocksY = pix.height / BLOCKSIZE


		For Local by:Int = 0 Until blocksY

			For Local bx:Int = 0 Until blocksX

				If nextCode >= codeCount Then Exit


				Local target:Long

				target = (Long(nextCode) * totalBlocks) / Long(codeCount)


				If target <= globalBlock Then

					CopyBlockToBook(book,nextCode,pix,bx,by,channel)

					nextCode :+ 1

				EndIf


				globalBlock :+ 1

			Next


			If nextCode >= codeCount Then Exit

		Next

	Next


	' ========================================================
	' PREENCHER CODES RESTANTES
	' ========================================================

	If nextCode > 0 Then

		Local trainedCodes:Int

		trainedCodes = nextCode


		While nextCode < codeCount

			Local sourceCode:Int

			sourceCode = nextCode Mod trainedCodes


			CopyCode(book,nextCode,sourceCode)


			nextCode :+ 1

		Wend

	Else

		Print "ERRO: nenhum bloco treinado."

		Return book

	EndIf


	BuildCodeBookIndex(book)


	Print ""
	Print "Codebook inicializado."


	' ========================================================
	' BUFFERS DO K-MEANS
	' ========================================================

	Local sums:Long[]
	Local counts:Long[]

	sums = New Long[codeCount * BLOCKPIXELS]
	counts = New Long[codeCount]


	Local block:Int[]

	block = New Int[BLOCKPIXELS]


	' ========================================================
	' 10 ITERACOES
	' ========================================================

	For Local iteration:Int = 1 To TRAIN_ITERATIONS

		Print ""
		Print "=========================================="
		Print "ITERACAO " + iteration + " / " + TRAIN_ITERATIONS
		Print "=========================================="


		' ------------------------------------------------------
		' LIMPAR ACUMULADORES
		' ------------------------------------------------------

		For Local i:Int = 0 Until sums.Length

			sums[i] = 0

		Next


		For Local i:Int = 0 Until counts.Length

			counts[i] = 0

		Next


		Local processed:Long

		processed = 0


		' ====================================================
		' PERCORRER DATASET
		' ====================================================

		For Local info:TImageInfo = EachIn images

			Local pix:TPixmap

			pix = LoadPixmap(info.path)


			If pix = Null Then

				Print "AVISO: nao consegui carregar " + info.path

				Continue

			EndIf


			Local blocksX:Int
			Local blocksY:Int

			blocksX = pix.width / BLOCKSIZE
			blocksY = pix.height / BLOCKSIZE


			For Local by:Int = 0 Until blocksY

				For Local bx:Int = 0 Until blocksX

					' ------------------------------------------
					' LER BLOCO
					' ------------------------------------------

					For Local yy:Int = 0 Until BLOCKSIZE

						For Local xx:Int = 0 Until BLOCKSIZE

							Local px:Int
							Local py:Int
							Local p:Int

							px = bx * BLOCKSIZE + xx
							py = by * BLOCKSIZE + yy

							p = yy * BLOCKSIZE + xx


							block[p] = GetPixelChannel(pix,px,py,channel)

						Next

					Next


					' ------------------------------------------
					' CODE MAIS PROXIMO
					' ------------------------------------------

					Local bestCode:Int

					bestCode = FindNearest(book,block)


					If bestCode < 0 Then

						bestCode = 0

					EndIf


					If bestCode >= codeCount Then

						bestCode = codeCount - 1

					EndIf


					' ------------------------------------------
					' ACUMULAR
					' ------------------------------------------

					Local base:Int

					base = bestCode * BLOCKPIXELS


					For Local p:Int = 0 Until BLOCKPIXELS

						sums[base + p] :+ Long(block[p])

					Next


					counts[bestCode] :+ 1


					processed :+ 1

				Next


				If by Mod 32 = 0 Then

					Print "  " + info.name + " linha " + by + " / " + blocksY

				EndIf

			Next

		Next


		Print "Blocos processados = " + processed


		' ====================================================
		' ATUALIZAR CODEBOOK
		' ====================================================

		Print "A atualizar codebook..."


		Local changedValues:Int

		changedValues = 0


		For Local code:Int = 0 Until codeCount

			If counts[code] <= 0 Then

				Continue

			EndIf


			Local base:Int

			base = code * BLOCKPIXELS


			For Local p:Int = 0 Until BLOCKPIXELS

				Local oldValue:Int
				Local newValue:Int

				oldValue = book.data[base + p]

				newValue = Int(sums[base + p] / counts[code])


				If newValue < 0 Then

					newValue = 0

				EndIf


				If newValue > 127 Then

					newValue = 127

				EndIf


				If oldValue <> newValue Then

					changedValues :+ 1

				EndIf


				book.data[base + p] = newValue

			Next

		Next


		Print "Valores alterados = " + changedValues


		' ====================================================
		' RECONSTRUIR HASH + INDICE
		' ====================================================

		BuildCodeBookIndex(book)


		Print "Iteracao " + iteration + " terminada."

	Next


	' ========================================================
	' TREINO TERMINADO
	' ========================================================

	Print ""
	Print "=========================================="
	Print "TREINO TERMINADO"
	Print "ITERACOES = " + TRAIN_ITERATIONS
	Print "=========================================="


	Return book

End Function


' ============================================================
' QUANTIZE RESIDUAL
' ============================================================

Function QuantizeResidual8:Int(value:Float,quantLevel:Int)

	If quantLevel <= 1 Then

		Local q:Int

		q = Int(value)


		If q < -128 Then q = -128

		If q > 127 Then q = 127


		Return q

	EndIf


	Local q:Int


	If value >= 0.0 Then

		q = Int((value + Float(quantLevel) * 0.5) / Float(quantLevel))

	Else

		q = -Int(((-value) + Float(quantLevel) * 0.5) / Float(quantLevel))

	EndIf


	If q < -128 Then q = -128

	If q > 127 Then q = 127


	Return q

End Function


' ============================================================
' WRITE U16
' ============================================================

Function WriteU16LE(stream:TStream,value:Int)

	WriteByte(stream,value & $FF)

	WriteByte(stream,(value Shr 8) & $FF)

End Function


' ============================================================
' WRITE U32
' ============================================================

Function WriteU32LE(stream:TStream,value:Int)

	WriteByte(stream,value & $FF)

	WriteByte(stream,(value Shr 8) & $FF)

	WriteByte(stream,(value Shr 16) & $FF)

	WriteByte(stream,(value Shr 24) & $FF)

End Function


' ============================================================
' WRITE U64
' ============================================================

Function WriteU64LE(stream:TStream,value:Long)

	For Local i:Int = 0 Until 8

		WriteByte(stream,Int(value & $FF))

		value = value Shr 8

	Next

End Function


' ============================================================
' WRITE ASCII
' ============================================================

Function WriteASCII(stream:TStream,text:String)

	For Local i:Int = 0 Until text.Length

		WriteByte(stream,text[i] & $FF)

	Next

End Function


' ============================================================
' SAVE SAFETENSOR
' ============================================================

Function SaveSafeTensor(book:TCodeBook,filename:String,tagName:String)

	Print "A gravar " + filename


	Local dataSize:Int

	dataSize = book.size * BLOCKPIXELS


	Local quote:String

	quote = Chr(34)


	Local header:String

	header = "{" + quote + tagName + quote + ":{" + quote + "dtype" + quote + ":" + quote + "U8" + quote + "," + quote + "shape" + quote + ":[" + book.size + "," + BLOCKPIXELS + "]," + quote + "data_offsets" + quote + ":[0," + dataSize + "]}}"


	Local stream:TStream

	stream = WriteStream(filename)


	If stream = Null Then

		Print "ERRO ao criar " + filename

		Return

	EndIf


	WriteU64LE(stream,Long(header.Length))

	WriteASCII(stream,header)


	For Local i:Int = 0 Until book.data.Length

		WriteByte(stream,book.data[i] & $FF)

	Next


	CloseStream(stream)


	Print "OK " + filename

End Function


' ============================================================
' IMAGE HEADER
' ============================================================

Function WriteImageHeader(stream:TStream,info:TImageInfo)

	WriteU32LE(stream,info.name.Length)

	WriteASCII(stream,info.name)

	WriteU32LE(stream,info.width)

	WriteU32LE(stream,info.height)

	WriteU32LE(stream,info.blocks)

End Function


' ============================================================
' Y RESIDUAL
' ============================================================

Function CalculateYResiduals:Int[](block:Int[],book:TCodeBook,code:Int)

	Local residuals:Int[]

	residuals = New Int[64]


	Local base:Int

	base = code * BLOCKPIXELS


	For Local p:Int = 0 Until 64

		residuals[p] = block[p] - book.data[base + p]

	Next


	Return residuals

End Function


' ============================================================
' C RESIDUAL
' ============================================================

Function CalculateCResiduals:Int[](block:Int[],book:TCodeBook,code:Int)

	Local residuals:Int[]

	residuals = New Int[4]


	Local base:Int

	base = code * BLOCKPIXELS


	For Local gy:Int = 0 Until 2

		For Local gx:Int = 0 Until 2

			Local total:Int

			total = 0


			For Local yy:Int = 0 Until 4

				For Local xx:Int = 0 Until 4

					Local x:Int
					Local y:Int
					Local p:Int

					x = gx * 4 + xx

					y = gy * 4 + yy

					p = y * 8 + x


					total :+ block[p] - book.data[base + p]

				Next

			Next


			Local index:Int

			index = gy * 2 + gx


			residuals[index] = total / 16

		Next

	Next


	Return residuals

End Function


' ============================================================
' GET C RESIDUAL
' ============================================================

Function GetCResidual:Int(residuals:Int[],x:Int,y:Int)

	Local gx:Int
	Local gy:Int
	Local index:Int

	gx = x / 4

	gy = y / 4

	index = gy * 2 + gx


	Return residuals[index]

End Function


' ============================================================
' ENCODE IMAGE
' ============================================================

Function EncodeImage:TPixmap(stream:TStream,info:TImageInfo,bookY:TCodeBook,bookU:TCodeBook,bookV:TCodeBook,qualityLevel:Int)

	Print ""
	Print "CODIFICAR: " + info.name


	Local pix:TPixmap

	pix = LoadPixmap(info.path)


	If pix = Null Then

		Print "ERRO ao carregar " + info.path

		Return Null

	EndIf


	Local output:TPixmap

	output = CreatePixmap(pix.width,pix.height,PF_RGB888)


	If output = Null Then

		Print "ERRO ao criar reconstruida."

		Return Null

	EndIf


	' ========================================================
	' QUANTIZACAO
	' ========================================================

	Local quantY:Int
	Local quantC:Int

	quantY = 64 - (qualityLevel * 63 / 100)

	quantC = 96 - (qualityLevel * 95 / 100)


	If quantY < 1 Then quantY = 1

	If quantC < 1 Then quantC = 1


	Print "Quant Y = " + quantY

	Print "Quant C = " + quantC


	' ========================================================
	' BUFFERS
	' ========================================================

	Local blockY:Int[]
	Local blockU:Int[]
	Local blockV:Int[]

	blockY = New Int[64]
	blockU = New Int[64]
	blockV = New Int[64]


	Local blocksX:Int
	Local blocksY:Int

	blocksX = pix.width / 8

	blocksY = pix.height / 8


	' ========================================================
	' BLOCOS
	' ========================================================

	For Local by:Int = 0 Until blocksY

		For Local bx:Int = 0 Until blocksX

			' ------------------------------------------------
			' LER BLOCO
			' ------------------------------------------------

			For Local yy:Int = 0 Until 8

				For Local xx:Int = 0 Until 8

					Local px:Int
					Local py:Int
					Local p:Int

					px = bx * 8 + xx

					py = by * 8 + yy

					p = yy * 8 + xx


					blockY[p] = GetPixelChannel(pix,px,py,0)

					blockU[p] = GetPixelChannel(pix,px,py,1)

					blockV[p] = GetPixelChannel(pix,px,py,2)

				Next

			Next


			' ------------------------------------------------
			' CODEBOOK
			' ------------------------------------------------

			Local codeY:Int
			Local codeU:Int
			Local codeV:Int

			codeY = FindNearest(bookY,blockY)

			codeU = FindNearest(bookU,blockU)

			codeV = FindNearest(bookV,blockV)


			' ------------------------------------------------
			' Y RESIDUAL
			' ------------------------------------------------

			Local rawY:Int[]

			rawY = CalculateYResiduals(blockY,bookY,codeY)


			Local residualY:Int[]

			residualY = New Int[64]


			For Local i:Int = 0 Until 64

				residualY[i] = QuantizeResidual8(Float(rawY[i]),quantY)

			Next


			' ------------------------------------------------
			' U / V RESIDUAL
			' ------------------------------------------------

			Local rawU:Int[]
			Local rawV:Int[]

			rawU = CalculateCResiduals(blockU,bookU,codeU)

			rawV = CalculateCResiduals(blockV,bookV,codeV)


			Local residualU:Int[]
			Local residualV:Int[]

			residualU = New Int[4]

			residualV = New Int[4]


			For Local i:Int = 0 Until 4

				residualU[i] = QuantizeResidual8(Float(rawU[i]),quantC)

				residualV[i] = QuantizeResidual8(Float(rawV[i]),quantC)

			Next


			' =================================================
			' HEADER DO BLOCO
			' =================================================

			WriteU16LE(stream,codeY)

			WriteU16LE(stream,codeU)

			WriteU16LE(stream,codeV)


			' =================================================
			' Y
			' =================================================

			For Local i:Int = 0 Until 64

				Local storedY:Int

				storedY = residualY[i] + 128

				WriteByte(stream,storedY & $FF)

			Next


			' =================================================
			' U
			' =================================================

			For Local i:Int = 0 Until 4

				Local storedU:Int

				storedU = residualU[i] + 128

				WriteByte(stream,storedU & $FF)

			Next


			' =================================================
			' V
			' =================================================

			For Local i:Int = 0 Until 4

				Local storedV:Int

				storedV = residualV[i] + 128

				WriteByte(stream,storedV & $FF)

			Next


			' =================================================
			' RECONSTRUCAO
			' =================================================

			Local baseY:Int
			Local baseU:Int
			Local baseV:Int

			baseY = codeY * 64

			baseU = codeU * 64

			baseV = codeV * 64


			For Local yy:Int = 0 Until 8

				For Local xx:Int = 0 Until 8

					Local p:Int

					p = yy * 8 + xx


					Local ry:Int

					ry = residualY[p] * quantY


					Local ci:Int

					ci = (yy / 4) * 2 + (xx / 4)


					Local ru:Int
					Local rv:Int

					ru = residualU[ci] * quantC

					rv = residualV[ci] * quantC


					Local yHalf:Int
					Local uHalf:Int
					Local vHalf:Int

					yHalf = bookY.data[baseY + p] + ry

					uHalf = bookU.data[baseU + p] + ru

					vHalf = bookV.data[baseV + p] + rv


					Local yv:Int
					Local uv:Int
					Local vv:Int

					yv = yHalf * 2

					uv = uHalf * 2

					vv = vHalf * 2


					yv = ClampByte(yv)

					uv = ClampByte(uv)

					vv = ClampByte(vv)


					Local r:Int
					Local g:Int
					Local b:Int

					r = YUVToR(yv,uv,vv)

					g = YUVToG(yv,uv,vv)

					b = YUVToB(yv,uv,vv)


					Local px:Int
					Local py:Int

					px = bx * 8 + xx

					py = by * 8 + yy


					Local color:Int

					color = r * 65536 + g * 256 + b


					WritePixel(output,px,py,color)

				Next

			Next

		Next


		If by Mod 16 = 0 Then

			Print "Linha " + by + " / " + blocksY

		EndIf

	Next


	Return output

End Function


' ============================================================
' SHOW RECONSTRUCTED
' ============================================================

Function ShowReconstructed(pix:TPixmap,name:String)

	If pix = Null Then Return


	Cls


	Local scaleX:Float
	Local scaleY:Float
	Local scale:Float

	scaleX = Float(GraphicsWidth() - 40) / Float(pix.width)

	scaleY = Float(GraphicsHeight() - 70) / Float(pix.height)


	scale = scaleX


	If scaleY < scale Then

		scale = scaleY

	EndIf


	If scale > 1.0 Then

		scale = 1.0

	EndIf


	If scale <= 0 Then

		scale = 1.0

	EndIf


	Local drawWidth:Int
	Local drawHeight:Int

	drawWidth = Int(Float(pix.width) * scale)

	drawHeight = Int(Float(pix.height) * scale)


	Local posX:Int
	Local posY:Int

	posX = (GraphicsWidth() - drawWidth) / 2

	posY = 40


	SetColor(255,255,255)


	DrawText("Reconstruida: " + name,10,10)

	DrawText("10 ITERACOES / SEM HAAR / SEM ZIGZAG / YUV / 2",10,25)


	SetScale(scale,scale)


	Local logicalX:Int
	Local logicalY:Int

	logicalX = Int(Float(posX) / scale)

	logicalY = Int(Float(posY) / scale)


	DrawPixmap(pix,logicalX,logicalY)


	SetScale(1.0,1.0)


	Flip

End Function


' ============================================================
' COPY FILE INTO STREAM
' ============================================================

Function CopyFileIntoStream:Int(output:TStream,filename:String)

	Local inputStream:TStream

	inputStream = ReadStream(filename)


	If inputStream = Null Then

		Print "ERRO ao abrir " + filename

		Return False

	EndIf


	Local bytesInFile:Long

	bytesInFile = StreamSize(inputStream)


	If bytesInFile < 0 Or bytesInFile > 2147483647 Then

		CloseStream(inputStream)

		Return False

	EndIf


	If bytesInFile > 0 Then

		CopyBytes(inputStream,output,Int(bytesInFile))

	EndIf


	CloseStream(inputStream)


	Return True

End Function


' ============================================================
' APPEND SAFETENSOR
' ============================================================

Function AppendSafeTensor:Int(output:TStream,filename:String,tag:String)

	If Not FileExists(filename) Then

		Print "ERRO: nao existe " + filename

		Return False

	EndIf


	Local fileBytes:Long

	fileBytes = FileSize(filename)


	If fileBytes < 0 Then

		Return False

	EndIf


	Print "A embutir " + filename

	Print "Bytes = " + fileBytes


	WriteASCII(output,tag)

	WriteU64LE(output,fileBytes)


	If Not CopyFileIntoStream(output,filename) Then

		Return False

	EndIf


	Return True

End Function


' ============================================================
' CREATE DATASET
' ============================================================

Function CreateDatasetDat(folder:String,images:TImageInfo[],bookY:TCodeBook,bookU:TCodeBook,bookV:TCodeBook,qualityLevel:Int)

	Local filename:String

	filename = JoinPath(folder,"dataset.dat")


	Print ""

	Print "=========================================="

	Print "CRIAR DATASET.DAT"

	Print "=========================================="


	Local stream:TStream

	stream = WriteStream(filename)


	If stream = Null Then

		Print "ERRO ao criar " + filename

		Return

	EndIf


	' ========================================================
	' MAGIC
	' ========================================================

	WriteASCII(stream,"DSET")


	' ========================================================
	' HEADER
	' ========================================================

	WriteU32LE(stream,HEADER_VERSION)

	WriteU32LE(stream,BLOCKSIZE)

	WriteU32LE(stream,BLOCK_DATA_BYTES)


	WriteU32LE(stream,bookY.size)

	WriteU32LE(stream,bookU.size)

	WriteU32LE(stream,bookV.size)


	WriteU32LE(stream,qualityLevel)

	WriteU32LE(stream,images.Length)


	' ========================================================
	' IMAGE HEADERS
	' ========================================================

	For Local info:TImageInfo = EachIn images

		WriteImageHeader(stream,info)

	Next


	' ========================================================
	' BLOCKS
	' ========================================================

	For Local info:TImageInfo = EachIn images

		Local reconstructed:TPixmap

		reconstructed = EncodeImage(stream,info,bookY,bookU,bookV,qualityLevel)


		If reconstructed <> Null Then

			ShowReconstructed(reconstructed,info.name)

		EndIf

	Next


	' ========================================================
	' SAFETENSORS
	' ========================================================

	WriteASCII(stream,"SAFE")


	If Not AppendSafeTensor(stream,JoinPath(folder,"datasetY.safetensors"),"SATY") Then

		Print "ERRO: SATY"

	EndIf


	If Not AppendSafeTensor(stream,JoinPath(folder,"datasetU.safetensors"),"SATU") Then

		Print "ERRO: SATU"

	EndIf


	If Not AppendSafeTensor(stream,JoinPath(folder,"datasetV.safetensors"),"SATV") Then

		Print "ERRO: SATV"

	EndIf


	WriteASCII(stream,"DONE")


	CloseStream(stream)


	Print ""

	Print "=========================================="

	Print "DATASET TERMINADO"

	Print "=========================================="


	Print "Criado:"

	Print filename


	Print ""

	Print "dataset.dat contem:"

	Print "  imagens"

	Print "  blocos"

	Print "  SATY"

	Print "  SATU"

	Print "  SATV"

End Function


' ============================================================
' MAIN
' ============================================================

Function Main()

	Print ""

	Print "=========================================="

	Print "DATASET CODEBOOK ENCODER"

	Print "BLITZMAX NG"

	Print "=========================================="

	Print ""


	Print "BLOCK       = 8x8"

	Print "BLOCKPIXELS = 64"

	Print ""

	Print "VERSION     = " + HEADER_VERSION

	Print "ITERACOES   = " + TRAIN_ITERATIONS

	Print "CODEBOOK Y  = " + CODEBOOKSIZEY

	Print "CODEBOOK U  = " + CODEBOOKSIZEC

	Print "CODEBOOK V  = " + CODEBOOKSIZEC

	Print ""


	Print "YUV         = RGB -> YUV -> /2"

	Print "HAAR        = OFF"

	Print "ZIGZAG      = OFF"

	Print ""


	Print "Y residual  = 64 bytes"

	Print "U residual  = 4 bytes"

	Print "V residual  = 4 bytes"

	Print ""


	Print "Bytes bloco = " + BLOCK_DATA_BYTES

	Print ""


	' ========================================================
	' FOLDER
	' ========================================================

	Local folder:String

	folder = Input("Folder das imagens: ")


	If folder = "" Then

		Print "Folder vazio."

		Return

	EndIf


	' ========================================================
	' QUALITY
	' ========================================================

	Local qualityLevel:Int

	qualityLevel = Int(Input("QUALITY 0-100: "))


	If qualityLevel < 0 Then

		qualityLevel = 0

	EndIf


	If qualityLevel > 100 Then

		qualityLevel = 100

	EndIf


	' ========================================================
	' IMAGENS
	' ========================================================

	Local files:String[]

	files = GetImageFiles(folder)


	If files = Null Or files.Length = 0 Then

		Print "Nao encontrei imagens."

		Return

	EndIf


	Print ""

	Print "Imagens encontradas: " + files.Length


	Local images:TImageInfo[]

	images = BuildImageList(folder,files)


	If images = Null Or images.Length = 0 Then

		Print "Nenhuma imagem valida."

		Return

	EndIf


	Local totalBlocks:Long

	totalBlocks = CountTotalBlocks(images)


	Print ""

	Print "Dataset = " + images.Length + " imagens"

	Print "Blocos = " + totalBlocks

	Print ""


	' ========================================================
	' CODEBOOKS
	'
	' TODOS treinados em YUV / 2
	'
	' Cada um passa por 10 iteracoes.
	' ========================================================

	Local bookY:TCodeBook
	Local bookU:TCodeBook
	Local bookV:TCodeBook


	Print ""
	Print "=========================================="
	Print "INICIO TREINO Y"
	Print "=========================================="


	bookY = TrainCodeBook(images,0,CODEBOOKSIZEY)


	Print ""
	Print "=========================================="
	Print "INICIO TREINO U"
	Print "=========================================="


	bookU = TrainCodeBook(images,1,CODEBOOKSIZEC)


	Print ""
	Print "=========================================="
	Print "INICIO TREINO V"
	Print "=========================================="


	bookV = TrainCodeBook(images,2,CODEBOOKSIZEC)


	' ========================================================
	' SAFE TENSORS
	' ========================================================

	Print ""

	Print "A gravar SafeTensors..."


	SaveSafeTensor(bookY,JoinPath(folder,"datasetY.safetensors"),"codebook")

	SaveSafeTensor(bookU,JoinPath(folder,"datasetU.safetensors"),"codebook")

	SaveSafeTensor(bookV,JoinPath(folder,"datasetV.safetensors"),"codebook")


	' ========================================================
	' DATASET
	' ========================================================

	CreateDatasetDat(folder,images,bookY,bookU,bookV,qualityLevel)


	Print ""

	Print "=========================================="

	Print "TUDO TERMINADO"

	Print "=========================================="


	Print ""

	Print "dataset.dat"

	Print "datasetY.safetensors"

	Print "datasetU.safetensors"

	Print "datasetV.safetensors"

	Print ""


	Print "TREINO = 10 ITERACOES"

	Print "SEM HAAR."

	Print "SEM ZIGZAG."

	Print "YUV / 2 aplicado no encoder."

	Print "YUV x 2 aplicado na reconstrução."

	Print ""

	Print "Os 3 SafeTensors estao EMBUTIDOS no dataset.dat."

	Print ""

	Print "ESC para sair."

End Function


' ============================================================
' RUN
' ============================================================

Main()


Repeat

	Delay(20)

Until KeyHit(KEY_ESCAPE)


EndGraphics