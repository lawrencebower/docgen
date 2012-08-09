package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.OverlayDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.model.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayPDFGenerator;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.File;
import java.util.Arrays;

import static org.junit.Assert.assertTrue;

public abstract class AbstractOverlayRendererTest extends AbstractIntegrationTest {

    @Autowired
    private OverlayDocumentInfo docInfo;

    @Autowired
    private OverlayPDFGenerator overlayGenerator;

    protected void createPDFAndCompareWithExpected(String expectedOutputFilePath,
                                                   String outFilePath,
                                                   String sourcePDF,
                                                   DocComponent... components) {

        docInfo.setSourcePDF(sourcePDF);
        docInfo.setComponents(Arrays.asList(components));
        docInfo.setName("Doc name");

        PDFDocument pdfDocument = overlayGenerator.generatePDF(docInfo);

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutputFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }
}
