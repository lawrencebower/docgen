package org.lawrencebower.docgen.core.generator.model;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class PDFDocumentImpl implements PDFDocument {

    private byte[] bytes;

    public PDFDocumentImpl(byte[] bytes) {
        this.bytes = bytes;
    }

    public void writeToFile(File file) {
        try {
            FileOutputStream outStream = new FileOutputStream(file);
            outStream.write(bytes);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
