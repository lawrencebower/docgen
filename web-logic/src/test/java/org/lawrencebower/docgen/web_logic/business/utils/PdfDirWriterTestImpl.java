package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.utils.PdfDirWriter;

import java.io.File;

public class PdfDirWriterTestImpl implements PdfDirWriter {

    @Override
    public File createPDFDir(String dirRoot) {
        File file = new File(dirRoot);

        if(!file.exists()){
            String message = String.format("Root directory '%s' does not exist?!", dirRoot);
            throw new DocGenException(message);
        }

        return file;//dont make a sub-dir, just return the root for test implementation
    }
}
