package org.lawrencebower.docgen.core.generator.model;

import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class PDFDocumentImpl implements PDFDocument {

    private byte[] bytes;
    private String name;
    private int copyNumber = 1;
    private File file;

    public PDFDocumentImpl(byte[] bytes) {
        this.bytes = bytes;
    }

    public void writeToFile(File file) {
        try {
            FileOutputStream outStream = new FileOutputStream(file);
            writeToStream(outStream);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    public void writeToStream(OutputStream stream) {
        try {
            stream.write(bytes);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    @Override
    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    public void setCopyNumber(int copyNumber) {
        this.copyNumber = copyNumber;
    }

    @Override
    public int getCopyNumber() {
        return copyNumber;
    }

    @Override
    public void setFile(File fileName) {
        this.file = fileName;
    }

    @Override
    public File getFile() {
        return file;
    }
}
