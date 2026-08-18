SuperStrict

Import BRL.StandardIO
Import BRL.Pixmap
Import BRL.JPGLoader
Import BRL.PNGLoader
Import BRL.Max2D


' ============================================================
' DATASET CODEBOOK DECODER
'
' FORMATO:
'
'   DSET
'   header
'   image headers
'   blocos
'   SAFE
'   SATY
'   SATU
'   SATV
'   DONE
'
'
' BLOCO = 78 bytes
'
'   codeY       U16 = 2
'   codeU       U16 = 2
'   codeV       U16 = 2
'
'   Y residual  64 bytes
'   U residual   4 bytes
'   V residual   4 bytes
'
'
' IMPORTANTE:
'
'   NAO HA HAAR
'   NAO HA ZIGZAG
'
'
' YUV:
'
'   ENCODER:
'
'       Y = YUV / 2
'       U = YUV / 2
'       V = YUV / 2
'
'   DECODER:
'
'       YUV = valor_decodificado * 2
'
'   SO DEPOIS:
'
'       YUV -> RGB
'
' ============================================================


Const BLOCKSIZE:Int = 8
Const BLOCKPIXELS:Int = 64
Const BLOCK_DATA_BYTES:Int = 78

Const HEADER_VERSION:Int = 6

Const JPEG_QUALITY:Int = 90


' ============================================================
' IMAGE INFO
' ============================================================

Type TImageInfo

	Field name:String
	Field width:Int
	Field height:Int
	Field blocks:Int

End Type


' ============================================================
' CODEBOOK
' ============================================================

Type TCodeBook

	Field size:Int
	Field data:Byte[]

End Type


' ============================================================
' CLAMP
' ============================================================

Function ClampByte:Int(value:Int)

	If value < 0 Then Return 0

	If value > 255 Then Return 255

	Return value

End Function


' ============================================================
' PATH
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
' READ U16 LE
' ============================================================

Function ReadU16LE:Int(stream:TStream)

	Local b0:Int
	Local b1:Int

	b0 = ReadByte(stream)
	b1 = ReadByte(stream)

	Return b0 | (b1 Shl 8)

End Function


' ============================================================
' READ U32 LE
' ============================================================

Function ReadU32LE:Int(stream:TStream)

	Local b0:Int
	Local b1:Int
	Local b2:Int
	Local b3:Int

	b0 = ReadByte(stream)
	b1 = ReadByte(stream)
	b2 = ReadByte(stream)
	b3 = ReadByte(stream)

	Return b0 | (b1 Shl 8) | (b2 Shl 16) | (b3 Shl 24)

End Function


' ============================================================
' READ U64 LE
' ============================================================

Function ReadU64LE:Long(stream:TStream)

	Local result:Long

	result = 0

	For Local i:Int = 0 Until 8

		Local b:Int

		b = ReadByte(stream)

		result :+ Long(b) Shl (i * 8)

	Next

	Return result

End Function


' ============================================================
' READ ASCII TAG
' ============================================================

Function ReadTag:String(stream:TStream,count:Int)

	Local result:String

	result = ""

	For Local i:Int = 0 Until count

		result :+ Chr(ReadByte(stream))

	Next

	Return result

End Function


' ============================================================
' CREATE CODEBOOK
' ============================================================

Function CreateCodeBook:TCodeBook(codeCount:Int)

	Local book:TCodeBook

	book = New TCodeBook

	book.size = codeCount
	book.data = New Byte[codeCount * BLOCKPIXELS]

	Return book

End Function


' ============================================================
' LOAD EMBEDDED SAFETENSOR
'
' Estrutura:
'
'   SATY
'   U64 fileSize
'   SafeTensor completo
'
' SafeTensor:
'
'   U64 headerLength
'   JSON
'   raw data
'
' ============================================================

Function LoadEmbeddedSafeTensor:TCodeBook(stream:TStream,tag:String,expectedCodes:Int)

	Print ""
	Print "------------------------------------------"
	Print "A carregar " + tag
	Print "------------------------------------------"


	Local actualTag:String

	actualTag = ReadTag(stream,4)


	If actualTag <> tag Then

		Print "ERRO: tag inesperada."

		Print "Esperado = " + tag
		Print "Recebido = " + actualTag

		Return Null

	EndIf


	Local FileSize:Long

	FileSize = ReadU64LE(stream)


	If FileSize < 8 Then

		Print "ERRO: tamanho SafeTensor invalido."

		Return Null

	EndIf


	Local fileStart:Long

	fileStart = StreamPos(stream)


	Local headerLength:Long

	headerLength = ReadU64LE(stream)


	If headerLength <= 0 Then

		Print "ERRO: header SafeTensor invalido."

		Return Null

	EndIf


	If headerLength > FileSize - 8 Then

		Print "ERRO: header SafeTensor demasiado grande."

		Return Null

	EndIf


	Local dataStart:Long

	dataStart = fileStart + 8 + headerLength


	Local dataSize:Long

	dataSize = FileSize - 8 - headerLength


	Local expectedSize:Long

	expectedSize = Long(expectedCodes) * Long(BLOCKPIXELS)


	Print "FileSize     = " + FileSize
	Print "Header       = " + headerLength
	Print "Data         = " + dataSize
	Print "Esperado     = " + expectedSize
	Print "Codes        = " + expectedCodes


	If dataSize <> expectedSize Then

		Print "ERRO: tamanho do codebook inesperado."

		Print "Esperado = " + expectedSize
		Print "Recebido = " + dataSize

		Return Null

	EndIf


	If expectedCodes <= 0 Then

		Print "ERRO: numero de codes invalido."

		Return Null

	EndIf


	Local book:TCodeBook

	book = CreateCodeBook(expectedCodes)


	SeekStream(stream,dataStart)


	For Local i:Int = 0 Until Int(dataSize)

		book.data[i] = Byte(ReadByte(stream))

	Next


	SeekStream(stream,fileStart + FileSize)


	Print "SafeTensor " + tag + " OK."

	Return book

End Function


' ============================================================
' IMAGE HEADER
' ============================================================

Function ReadImageHeader:TImageInfo(stream:TStream)

	Local info:TImageInfo

	info = New TImageInfo


	Local nameLength:Int

	nameLength = ReadU32LE(stream)


	If nameLength < 0 Or nameLength > 1048576 Then

		Print "ERRO: nome de imagem invalido."

		Return Null

	EndIf


	info.name = ReadTag(stream,nameLength)

	info.width = ReadU32LE(stream)

	info.height = ReadU32LE(stream)

	info.blocks = ReadU32LE(stream)


	If info.width <= 0 Or info.height <= 0 Then

		Print "ERRO: dimensoes invalidas."

		Return Null

	EndIf


	If info.blocks <= 0 Then

		Print "ERRO: numero de blocos invalido."

		Return Null

	EndIf


	Return info

End Function


' ============================================================
' VALIDATE FORMAT
' ============================================================

Function ValidateFormat:Int(version:Int,blockSize:Int,blockBytes:Int)

	If version <> HEADER_VERSION Then

		Print "ERRO: versao incompativel."

		Print "Dataset = " + version
		Print "Esperado = " + HEADER_VERSION

		Return False

	EndIf


	If blockSize <> BLOCKSIZE Then

		Print "ERRO: BLOCKSIZE incompativel."

		Print "Dataset = " + blockSize
		Print "Esperado = " + BLOCKSIZE

		Return False

	EndIf


	If blockBytes <> BLOCK_DATA_BYTES Then

		Print "ERRO: BLOCK_DATA_BYTES incompativel."

		Print "Dataset = " + blockBytes
		Print "Esperado = " + BLOCK_DATA_BYTES

		Return False

	EndIf


	Return True

End Function


' ============================================================
' YUV -> RGB
'
' RECEBE YUV NORMAL 0..255
'
' O /2 JA FOI DESFEITO ANTES DE CHEGAR AQUI.
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
' MAKE OUTPUT NAME
' ============================================================

Function MakeOutputName:String(name:String,index:Int)

	Local result:String

	result = name


	Local dot:Int

	dot = result.FindLast(".")


	If dot >= 0 Then

		result = result[..dot]

	EndIf


	result :+ "_decoded.jpg"


	Return result

End Function


' ============================================================
' SAVE JPEG
' ============================================================

Function SaveDecodedImage:Int(pix:TPixmap,folder:String,name:String,index:Int)

	If pix = Null Then Return False


	Local outputName:String

	outputName = MakeOutputName(name,index)


	Local outputPath:String

	outputPath = JoinPath(folder,outputName)


	Print ""
	Print "A gravar:"
	Print outputPath


	If SavePixmapJPeg(pix,outputPath,JPEG_QUALITY) Then

		Print "JPEG OK."

		Return True

	EndIf


	Print "ERRO ao gravar JPEG."

	Return False

End Function


' ============================================================
' DECODE IMAGE
'
' SEM HAAR
' SEM ZIGZAG
'
' Cada residual Y corresponde diretamente:
'
'   residualY[0] -> pixel 0
'   residualY[1] -> pixel 1
'   ...
'
' ============================================================

Function DecodeImage:TPixmap(stream:TStream,info:TImageInfo,bookY:TCodeBook,bookU:TCodeBook,bookV:TCodeBook,qualityLevel:Int)

	Print ""
	Print "=========================================="
	Print "DESCODIFICAR: " + info.name
	Print "=========================================="

	Print "Size    = " + info.width + " x " + info.height
	Print "Blocos  = " + info.blocks
	Print "Quality = " + qualityLevel
	Print "Haar    = OFF"
	Print "ZigZag  = OFF"
	Print "YUV /2  = ON"


	' --------------------------------------------------------
	' PIXMAP
	' --------------------------------------------------------

	Local output:TPixmap

	output = CreatePixmap(info.width,info.height,PF_RGB888)


	If output = Null Then

		Print "ERRO: nao consegui criar pixmap."

		Return Null

	EndIf


	' --------------------------------------------------------
	' QUANTIZAÇÃO
	'
	' TEM DE SER A MESMA DO ENCODER
	' --------------------------------------------------------

	Local quantY:Int
	Local quantC:Int

	quantY = 64 - (qualityLevel * 63 / 100)

	quantC = 96 - (qualityLevel * 95 / 100)


	If quantY < 1 Then quantY = 1

	If quantC < 1 Then quantC = 1


	Print "Quant Y = " + quantY
	Print "Quant C = " + quantC


	' --------------------------------------------------------
	' ARRAYS
	' --------------------------------------------------------

	Local residualY:Int[]
	Local residualU:Int[]
	Local residualV:Int[]

	residualY = New Int[64]

	residualU = New Int[4]

	residualV = New Int[4]


	' --------------------------------------------------------
	' BLOCO
	' --------------------------------------------------------

	Local blocksX:Int
	Local blocksY:Int

	blocksX = info.width / BLOCKSIZE
	blocksY = info.height / BLOCKSIZE


	For Local by:Int = 0 Until blocksY

		For Local bx:Int = 0 Until blocksX


			' ================================================
			' CODES
			' ================================================

			Local codeY:Int
			Local codeU:Int
			Local codeV:Int


			codeY = ReadU16LE(stream)

			codeU = ReadU16LE(stream)

			codeV = ReadU16LE(stream)


			If codeY < 0 Or codeY >= bookY.size Then

				Print "ERRO: codeY invalido = " + codeY

				Return Null

			EndIf


			If codeU < 0 Or codeU >= bookU.size Then

				Print "ERRO: codeU invalido = " + codeU

				Return Null

			EndIf


			If codeV < 0 Or codeV >= bookV.size Then

				Print "ERRO: codeV invalido = " + codeV

				Return Null

			EndIf


			' ================================================
			' Y RESIDUAL
			'
			' DIRETO.
			'
			' NAO HA HAAR.
			' NAO HA ZIGZAG.
			' ================================================

			For Local i:Int = 0 Until 64

				Local stored:Int

				stored = ReadByte(stream)


				residualY[i] = stored - 128

			Next


			' ================================================
			' U RESIDUAL
			' ================================================

			For Local i:Int = 0 Until 4

				Local stored:Int

				stored = ReadByte(stream)

				residualU[i] = stored - 128

			Next


			' ================================================
			' V RESIDUAL
			' ================================================

			For Local i:Int = 0 Until 4

				Local stored:Int

				stored = ReadByte(stream)

				residualV[i] = stored - 128

			Next


			' ================================================
			' CODEBOOK BASE
			' ================================================

			Local baseY:Int
			Local baseU:Int
			Local baseV:Int

			baseY = codeY * BLOCKPIXELS
			baseU = codeU * BLOCKPIXELS
			baseV = codeV * BLOCKPIXELS


			' ================================================
			' PIXELS
			' ================================================

			For Local yy:Int = 0 Until 8

				For Local xx:Int = 0 Until 8


					Local p:Int

					p = yy * 8 + xx


					' ----------------------------------------
					' RESIDUAL Y
					' ----------------------------------------

					Local ry:Int

					ry = residualY[p] * quantY


					' ----------------------------------------
					' RESIDUAL U/V
					' ----------------------------------------

					Local ci:Int

					ci = (yy / 4) * 2 + (xx / 4)


					Local ru:Int
					Local rv:Int

					ru = residualU[ci] * quantC

					rv = residualV[ci] * quantC


					' ----------------------------------------
					' CODEBOOK
					'
					' O CODEBOOK ESTA EM YUV / 2
					' ----------------------------------------

					Local yHalf:Int
					Local uHalf:Int
					Local vHalf:Int


					yHalf = Int(bookY.data[baseY + p] & $FF) + ry

					uHalf = Int(bookU.data[baseU + p] & $FF) + ru

					vHalf = Int(bookV.data[baseV + p] & $FF) + rv


					' ----------------------------------------
					' DESFAZER /2
					'
					' ENCODER:
					'
					'   Y = Y / 2
					'   U = U / 2
					'   V = V / 2
					'
					' DECODER:
					'
					'   Y = Y * 2
					'   U = U * 2
					'   V = V * 2
					' ----------------------------------------

					Local yv:Int
					Local uv:Int
					Local vv:Int


					yv = yHalf * 2

					uv = uHalf * 2

					vv = vHalf * 2


					' ----------------------------------------
					' CLAMP
					' ----------------------------------------

					yv = ClampByte(yv)

					uv = ClampByte(uv)

					vv = ClampByte(vv)


					' ----------------------------------------
					' YUV -> RGB
					' ----------------------------------------

					Local r:Int
					Local g:Int
					Local b:Int


					r = YUVToR(yv,uv,vv)

					g = YUVToG(yv,uv,vv)

					b = YUVToB(yv,uv,vv)


					' ----------------------------------------
					' OUTPUT PIXEL
					' ----------------------------------------

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


	Print "OK: " + info.name


	Return output

End Function


' ============================================================
' PREVIEW
' ============================================================

Function PreviewImage(reconstructed:TPixmap,info:TImageInfo,index:Int,total:Int)

	If reconstructed = Null Then Return


	Cls


	Local scaleX:Float
	Local scaleY:Float
	Local scale:Float


	scaleX = Float(GraphicsWidth() - 40) / Float(reconstructed.width)

	scaleY = Float(GraphicsHeight() - 90) / Float(reconstructed.height)


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


	drawWidth = Int(Float(reconstructed.width) * scale)

	drawHeight = Int(Float(reconstructed.height) * scale)


	Local posX:Int
	Local posY:Int


	posX = (GraphicsWidth() - drawWidth) / 2

	posY = 70


	SetScale(1.0,1.0)

	SetColor(255,255,255)


	DrawText("Reconstruida: " + info.name,10,10)

	DrawText("Imagem " + (index + 1) + " / " + total,10,30)

	DrawText("YUV /2 -> x2 | Haar OFF | ZigZag OFF",10,50)


	SetScale(scale,scale)


	Local logicalX:Int
	Local logicalY:Int


	logicalX = Int(Float(posX) / scale)

	logicalY = Int(Float(posY) / scale)


	DrawPixmap(reconstructed,logicalX,logicalY)


	SetScale(1.0,1.0)


	Flip

End Function


' ============================================================
' MAIN
' ============================================================

Function Main()

	Print ""
	Print "=========================================="
	Print "DATASET CODEBOOK DECODER"
	Print "BLITZMAX NG"
	Print "=========================================="
	Print ""

	Print "BLOCK        = 8x8"
	Print "BLOCKPIXELS  = 64"
	Print "BLOCK DATA   = 78 bytes"
	Print "VERSION      = 6"
	Print "HAAR         = OFF"
	Print "ZIGZAG       = OFF"
	Print "YUV /2       = ON"
	Print "OUTPUT       = JPEG"
	Print "JPEG QUALITY = " + JPEG_QUALITY
	Print ""


	' ========================================================
	' INPUT
	' ========================================================

	Local filename:String


	If AppArgs.Length >= 2 Then

		filename = AppArgs[1]

	Else

		filename = Input("dataset.dat: ")

	EndIf


	If filename = "" Then

		Print "Nenhum ficheiro."

		Return

	EndIf


	If Not FileExists(filename) Then

		Print "ERRO: ficheiro nao existe:"

		Print filename

		Return

	EndIf


	' ========================================================
	' OUTPUT FOLDER
	' ========================================================

	Local folder:String

	folder = ExtractDir(filename)


	If folder = "" Then

		folder = CurrentDir()

	EndIf


	Local outputFolder:String

	outputFolder = JoinPath(folder,"decoded")


	CreateDir(outputFolder)


	Print ""
	Print "Input:"
	Print filename


	Print ""
	Print "Output:"
	Print outputFolder


	' ========================================================
	' OPEN DATASET
	' ========================================================

	Local stream:TStream

	stream = ReadStream(filename)


	If stream = Null Then

		Print "ERRO ao abrir dataset."

		Return

	EndIf


	' ========================================================
	' MAGIC
	' ========================================================

	Local magic:String

	magic = ReadTag(stream,4)


	If magic <> "DSET" Then

		Print "ERRO: dataset invalido."

		Print "Magic = " + magic

		CloseStream(stream)

		Return

	EndIf


	Print "DSET OK."


	' ========================================================
	' HEADER
	' ========================================================

	Local version:Int
	Local blockSize:Int
	Local blockBytes:Int

	Local codebookYSize:Int
	Local codebookUSize:Int
	Local codebookVSize:Int

	Local qualityLevel:Int
	Local imageCount:Int


	version = ReadU32LE(stream)

	blockSize = ReadU32LE(stream)

	blockBytes = ReadU32LE(stream)


	codebookYSize = ReadU32LE(stream)

	codebookUSize = ReadU32LE(stream)

	codebookVSize = ReadU32LE(stream)


	qualityLevel = ReadU32LE(stream)

	imageCount = ReadU32LE(stream)


	Print ""
	Print "VERSION     = " + version
	Print "BLOCKSIZE   = " + blockSize
	Print "BLOCK BYTES = " + blockBytes

	Print "CODEBOOK Y  = " + codebookYSize
	Print "CODEBOOK U  = " + codebookUSize
	Print "CODEBOOK V  = " + codebookVSize

	Print "QUALITY     = " + qualityLevel
	Print "IMAGES      = " + imageCount


	' ========================================================
	' VALIDATE
	' ========================================================

	If Not ValidateFormat(version,blockSize,blockBytes) Then

		CloseStream(stream)

		Return

	EndIf


	If imageCount <= 0 Then

		Print "ERRO: dataset sem imagens."

		CloseStream(stream)

		Return

	EndIf


	If imageCount > 100000 Then

		Print "ERRO: imageCount demasiado grande."

		CloseStream(stream)

		Return

	EndIf


	If codebookYSize <= 0 Or codebookYSize > 65535 Then

		Print "ERRO: codebook Y invalido."

		CloseStream(stream)

		Return

	EndIf


	If codebookUSize <= 0 Or codebookUSize > 65535 Then

		Print "ERRO: codebook U invalido."

		CloseStream(stream)

		Return

	EndIf


	If codebookVSize <= 0 Or codebookVSize > 65535 Then

		Print "ERRO: codebook V invalido."

		CloseStream(stream)

		Return

	EndIf


	' ========================================================
	' IMAGE HEADERS
	' ========================================================

	Local images:TImageInfo[]

	images = New TImageInfo[imageCount]


	Print ""
	Print "HEADERS DAS IMAGENS"


	For Local i:Int = 0 Until imageCount


		images[i] = ReadImageHeader(stream)


		If images[i] = Null Then

			Print "ERRO no header da imagem " + i

			CloseStream(stream)

			Return

		EndIf


		Print ""
		Print "Imagem " + i

		Print "  Nome   = " + images[i].name

		Print "  Size   = " + images[i].width + " x " + images[i].height

		Print "  Blocos = " + images[i].blocks


		If images[i].width Mod BLOCKSIZE <> 0 Then

			Print "ERRO: largura nao multipla de 8."

			CloseStream(stream)

			Return

		EndIf


		If images[i].height Mod BLOCKSIZE <> 0 Then

			Print "ERRO: altura nao multipla de 8."

			CloseStream(stream)

			Return

		EndIf


		Local expectedBlocks:Int

		expectedBlocks = (images[i].width / 8) * (images[i].height / 8)


		If images[i].blocks <> expectedBlocks Then

			Print "ERRO: numero de blocos invalido."

			Print "Header  = " + images[i].blocks

			Print "Esperado = " + expectedBlocks

			CloseStream(stream)

			Return

		EndIf


	Next


	' ========================================================
	' BLOCK DATA START
	' ========================================================

	Local blockDataStart:Long

	blockDataStart = StreamPos(stream)


	Print ""
	Print "BlockDataStart = " + blockDataStart


	' ========================================================
	' CALCULATE TOTAL BLOCK DATA
	' ========================================================

	Local totalBlockBytes:Long

	totalBlockBytes = 0


	For Local i:Int = 0 Until imageCount

		totalBlockBytes :+ Long(images[i].blocks) * Long(BLOCK_DATA_BYTES)

	Next


	Print "Total block bytes = " + totalBlockBytes


	' ========================================================
	' SAFE POSITION
	' ========================================================

	Local safePosition:Long

	safePosition = blockDataStart + totalBlockBytes


	Print "SAFE position = " + safePosition


	' ========================================================
	' SEEK SAFE
	' ========================================================

	SeekStream(stream,safePosition)


	' ========================================================
	' SAFE
	' ========================================================

	Local safeTag:String

	safeTag = ReadTag(stream,4)


	If safeTag <> "SAFE" Then

		Print "ERRO: SAFE nao encontrado."

		Print "Encontrado = " + safeTag

		CloseStream(stream)

		Return

	EndIf


	Print ""
	Print "SAFE OK."


	' ========================================================
	' SATY
	' ========================================================

	Local bookY:TCodeBook

	bookY = LoadEmbeddedSafeTensor(stream,"SATY",codebookYSize)


	If bookY = Null Then

		CloseStream(stream)

		Return

	EndIf


	' ========================================================
	' SATU
	' ========================================================

	Local bookU:TCodeBook

	bookU = LoadEmbeddedSafeTensor(stream,"SATU",codebookUSize)


	If bookU = Null Then

		CloseStream(stream)

		Return

	EndIf


	' ========================================================
	' SATV
	' ========================================================

	Local bookV:TCodeBook

	bookV = LoadEmbeddedSafeTensor(stream,"SATV",codebookVSize)


	If bookV = Null Then

		CloseStream(stream)

		Return

	EndIf


	' ========================================================
	' DONE
	' ========================================================

	Local doneTag:String

	doneTag = ReadTag(stream,4)


	If doneTag = "DONE" Then

		Print "DONE OK."

	Else

		Print "AVISO: DONE esperado."

		Print "Encontrado = " + doneTag

	EndIf


	' ========================================================
	' VOLTAR AO PRIMEIRO BLOCO
	' ========================================================

	Print ""
	Print "Voltar para os blocos..."

	SeekStream(stream,blockDataStart)


	' ========================================================
	' GRAPHICS
	' ========================================================

	Graphics 1920,1080


	' ========================================================
	' DESCODIFICAR
	' ========================================================

	For Local i:Int = 0 Until imageCount


		Print ""
		Print "##########################################"

		Print "IMAGEM " + (i + 1) + " / " + imageCount

		Print "##########################################"


		Local reconstructed:TPixmap


		reconstructed = DecodeImage(stream,images[i],bookY,bookU,bookV,qualityLevel)


		If reconstructed = Null Then

			Print "ERRO na reconstrução."

			CloseStream(stream)

			EndGraphics

			Return

		EndIf


		' ====================================================
		' SAVE
		' ====================================================

		If SaveDecodedImage(reconstructed,outputFolder,images[i].name,i) Then

			Print "JPEG guardado."

		Else

			Print "ERRO ao guardar JPEG."

		EndIf


		' ====================================================
		' PREVIEW
		' ====================================================

		PreviewImage(reconstructed,images[i],i,imageCount)


		Print ""
		Print "Imagem " + (i + 1) + " terminada."


		reconstructed = Null


	Next


	' ========================================================
	' END
	' ========================================================

	CloseStream(stream)


	Print ""
	Print "=========================================="
	Print "DESCODIFICACAO TERMINADA"
	Print "=========================================="

	Print ""
	Print "Total de imagens: " + imageCount

	Print ""
	Print "JPEGs gravados em:"
	Print outputFolder

	Print ""
	Print "Processamento concluido."


	EndGraphics

End Function


Main()