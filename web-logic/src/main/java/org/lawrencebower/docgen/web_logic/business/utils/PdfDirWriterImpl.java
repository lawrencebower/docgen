package org.lawrencebower.docgen.web_logic.business.utils;

import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.web_model.business_def.utils.PdfDirWriter;

import java.io.File;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

public class PdfDirWriterImpl implements PdfDirWriter {

    private DateFormat format = new SimpleDateFormat("yyyy_MM_dd-HH-mm-ss");

    @Override
    public File createPDFDir(String dirRoot) {

        checkRootExists(dirRoot);

        return createPDFFile(dirRoot);
    }

    private File createPDFFile(String dirRoot) {

        String dirName = createDirName();

        return makeDirIfNecessaryAndCheck(dirRoot, dirName);
    }

    private File makeDirIfNecessaryAndCheck(String dirRoot, String dirName) {

        String pathname = dirRoot + File.separator + dirName;

        File pdfDir = new File(pathname);

        boolean success = pdfDir.mkdir();

        if(!success){
            String path = pdfDir.getPath();
            String message = String.format("Problem creating directory - '%s'", path);
            throw new DocGenException(message);
        }

        return pdfDir;
    }

    private String createDirName() {
        Calendar instance = Calendar.getInstance();
        Date time = instance.getTime();
        return format.format(time);
    }

    private void checkRootExists(String dirRoot) {
        File rootDir = new File(dirRoot);

        if(!rootDir.exists()){
            String message = String.format("Directory root '%s' does not exist?!", dirRoot);
            throw new DocGenException(message);
        }
    }
}
