package org.lawrencebower.docgen.core.document;

import java.io.File;
import java.io.OutputStream;

public interface PDFDocument {

    void writeToFile(File file);

    void writeToStream(OutputStream stream);

    void setName(String name);

    String getName();

    void setNameExtension(String nameExtension);

    String getNameExtension();

    int getCopyNumber();

    void setCopyNumber(int copyNumber);

    void setFile(File fileName);

    File getFile();
}
