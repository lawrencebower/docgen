package org.lawrencebower.docgen.core.document;

import java.io.File;
import java.io.OutputStream;

public interface PDFDocument {

    public void writeToFile(File file);

    public void writeToStream(OutputStream stream);

    void setName(String name);

    String getName();
}
