package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayPDFGenerator;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponentFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertTrue;

public abstract class AbstractOverlayRendererTest extends AbstractIntegrationTest {

    @Autowired
    private OverlayComponentFactory componentFactory;
    @Autowired
    private OverlayPDFGenerator overlayGenerator;

    protected void createPDFAndCompareWithExpected(String expectedOutputFilePath,
                                                   String outFilePath,
                                                   String sourcePDF,
                                                   DocComponent... components) {

        OverlayDocument document = new OverlayDocument("Doc name", overlayGenerator);

        document.setSourcePDF(sourcePDF);

        List<OverlayComponent> overlayComponents = convertComponents(components);

        document.setComponents(overlayComponents);

        PDFDocument pdfDocument = overlayGenerator.generatePDF(document);

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutputFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }

    private List<OverlayComponent> convertComponents(DocComponent[] components) {

        List<OverlayComponent> results = new ArrayList<>();

        for (DocComponent component : components) {
            results.add(createOverlayComponent(component));
        }

        return results;
    }

    protected OverlayComponent createOverlayComponent(DocComponent component){
        return componentFactory.createOverlayComponent(component);
    }
}
