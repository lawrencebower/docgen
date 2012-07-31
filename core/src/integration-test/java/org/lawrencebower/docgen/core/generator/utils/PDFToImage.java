package org.lawrencebower.docgen.core.generator.utils;

/**
 * Copyright (c) 2005-2006, www.pdfbox.org
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. Neither the name of pdfbox; nor the names of its
 *    contributors may be used to endorse or promote products derived from this
 *    software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * http://www.pdfbox.org
 *
 */

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.pdfbox.pdmodel.PDDocument;
import org.pdfbox.pdmodel.PDPage;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

/**
 * Simplified version of PDFToImage from http://www.pdfbox.org (release 0.7.3)
 */
public class PDFToImage {

    public static void pdfToImage(File pdfFile, String outPutPrefix) {

        PDDocument document = null;

        try {

            document = PDDocument.load(pdfFile);

            writeDocumentAsImage(outPutPrefix, document);

        } catch (IOException e) {
            throw new DocGenException(e);
        } finally {
            closeDocument(document);
        }
    }

    private static void closeDocument(PDDocument document) {
        if (document != null) {
            try {
                document.close();
            } catch (IOException e) {
                throw new DocGenException(e);
            }
        }
    }

    private static void writeDocumentAsImage(String putputPrefix, PDDocument document) {

        String imageType = "png";
        int startPage = 1;
        int endPage = Integer.MAX_VALUE;

        List pages = document.getDocumentCatalog().getAllPages();

        for (int i = startPage - 1; i < endPage && i < pages.size(); i++) {

            ImageOutputStream output = null;
            ImageWriter imageWriter = null;

            try {
                PDPage page = (PDPage) pages.get(i);
                BufferedImage image = page.convertToImage();
                String fileName = putputPrefix + (i + 1) + "." + imageType;
                System.out.println("Writing:" + fileName);
                output = ImageIO.createImageOutputStream(new File(fileName));

                boolean foundWriter = false;
                Iterator writerIter = ImageIO.getImageWritersByFormatName(imageType);

                while (writerIter.hasNext() && !foundWriter) {
                    try {
                        imageWriter = (ImageWriter) writerIter.next();
                        ImageWriteParam writerParams = imageWriter.getDefaultWriteParam();
                        if (writerParams.canWriteCompressed()) {
                            writerParams.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
                            writerParams.setCompressionQuality(1.0f);
                        }


                        imageWriter.setOutput(output);
                        imageWriter.write(null, new IIOImage(image, null, null), writerParams);
                        foundWriter = true;
                    } catch (IOException io) {
                        throw new DocGenException(io);
                    } finally {
                        if (imageWriter != null) {
                            imageWriter.dispose();
                        }
                    }
                }
                if (!foundWriter) {
                    throw new RuntimeException("Error: no writer found for image type '" + imageType + "'");
                }
            } catch (IOException e) {
                throw new DocGenException(e);
            } finally {
                if (output != null) {
                    try {
                        output.flush();
                        output.close();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        }
    }

}
