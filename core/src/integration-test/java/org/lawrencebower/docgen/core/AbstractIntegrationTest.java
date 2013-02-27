package org.lawrencebower.docgen.core;

import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.utils.ChecksumUtils;
import org.lawrencebower.docgen.core.generator.utils.DocGenFileUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

import java.io.File;

public abstract class AbstractIntegrationTest {

    @Autowired
    protected DocGenFileUtils fileUtils;

    @Autowired
    protected ChecksumUtils checksumUtils;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String testOutputRoot;

    protected String inputPackage;
    protected String outputPackage;

    protected void prepareDirs() {

        String packageName = getClass().getPackage().getName();
        packageName = packageName.replaceAll("\\.", "\\\\");

        inputPackage = packageName + File.separator;
        outputPackage = testOutputRoot + packageName + File.separator;

        makeOutputDirs(outputPackage);
    }

    private void makeOutputDirs(String outputPackage) {
        new File(outputPackage).mkdirs();
    }

    protected File createOutputFilePathAndWriteFile(String outputFilePath, PDFDocument pdfDocument) {

        File outputFile = new File(outputFilePath);

        fileUtils.deleteQuietly(outputFile);

        pdfDocument.writeToFile(outputFile);

        return outputFile;
    }

    protected File classPathResourceToFile(String path) {
        return fileUtils.classPathResourceToFile(path);
    }
}
