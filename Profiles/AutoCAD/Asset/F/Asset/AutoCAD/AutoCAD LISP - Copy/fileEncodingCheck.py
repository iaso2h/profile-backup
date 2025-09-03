from pathlib import Path
import os
import chardet
import codecs

def printAllEncoding():
    for f in Path(os.getcwd()).iterdir():
        if f.suffix == ".lsp":
            with open(str(f), mode="rb+") as lspFile:
                contentBinary = lspFile.read()
                result = chardet.detect(contentBinary)
                if result["encoding"] != "gb2312":
                    print(f'Path: {str(f)}, Encoding: {result["encoding"]}, Confidence: {result["confidence"]:.2f}')
                    # Try to decode with GB2312 (this will fail if the file isn't already in GB2312 or compatible)
                    try:
                        contentDecoded = contentBinary.decode("gb2312")
                        print("Successfully decoded with GB2312")
                        contentEncoded = contentDecoded.encode("gb2312")
                        lspFile.write(contentEncoded)
                        print("File written with GB2312 encoding")
                    except UnicodeDecodeError:
                        # If it fails, try to detect the encoding first
                        print(f"Detected encoding: {result['encoding']} with confidence: {result['confidence']:.2f}")
                        # contentBinary = contentBinary.decode(result["encoding"])

    # Loop again to check the encoding
    for f in Path(os.getcwd()).iterdir():
        if f.suffix == ".lsp":
            with open(str(f), mode="rb") as lspFile:
                contentBinary = lspFile.read()
                result = chardet.detect(contentBinary)
                if result["encoding"] != "GB2312":
                    print(f'Path: {str(f)}, Encoding: {result["encoding"]}, Confidence: {result["confidence"]:.2f}')

printAllEncoding()

# Verify the encoding
# with open(file_path, mode="rb") as lspFileDemo:
#     result = chardet.detect(lspFileDemo.read())
#     print(f"After writing, detected encoding: {result['encoding']}, Confidence: {result['confidence']:.2f}")
