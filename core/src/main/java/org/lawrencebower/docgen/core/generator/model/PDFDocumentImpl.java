package org.lawrencebower.docgen.core.generator.model;

import org.apache.commons.lang.StringUtils;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.FilePermissionSetter;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;

public class PDFDocumentImpl implements PDFDocument {

    private byte[] bytes;
    private String name;
    private String nameExtension;
    private int copyNumber = 1;
    private File file;

    public PDFDocumentImpl(byte[] bytes) {
        this.bytes = bytes;
    }

    public void writeToFile(File file) {
        try {
            FileOutputStream outStream = new FileOutputStream(file);
            writeToStream(outStream);
            setFilePermissions(file);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    private void setFilePermissions(File file) {
        FilePermissionSetter setter = FilePermissionSetter.getPermissionSetter();
        setter.setFilePermissions(file);
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

    @Override
    public void setNameExtension(String nameExtension) {
        this.nameExtension = nameExtension;
    }

    public String getNameExtension() {

        String returnString = "";

        if (StringUtils.isNotBlank(nameExtension)) {
            returnString = "_" + nameExtension;
        }

        return returnString;
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
        file = fileName;
    }

    @Override
    public File getFile() {
        return file;
    }
}
