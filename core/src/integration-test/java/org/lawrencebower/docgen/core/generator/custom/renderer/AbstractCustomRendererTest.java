package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomDocument;
import org.lawrencebower.docgen.core.generator.custom.CustomPDFGenerator;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponent;
import org.lawrencebower.docgen.core.generator.custom.component.CustomComponentFactory;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertTrue;

public abstract class AbstractCustomRendererTest extends AbstractIntegrationTest {

    @Autowired
    private CustomPDFGenerator customGenerator;
    @Autowired
    private CustomComponentFactory componentFactory;

    protected AbstractCustomRendererTest() {
    }

    protected void createPDFAndCompareWithExpected(String expectedOutputFilePath,
                                                   String outFilePath,
                                                   DocComponent... components) {

        CustomDocument document = new CustomDocument("Doc name", customGenerator);

        List<CustomComponent> overlayComponents = convertComponents(components);

        document.setComponents(overlayComponents);

        PDFDocument pdfDocument = document.generatePDF();

        File outputFile = createOutputFilePathAndWriteFile(outFilePath, pdfDocument);

        File expectedFile = new File(expectedOutputFilePath);

        boolean fileSameAsExpected = checksumUtils.filteredFileChecksumsAreSame(expectedFile, outputFile);

        assertTrue(fileSameAsExpected);
    }

    private List<CustomComponent> convertComponents(DocComponent[] components) {

        List<CustomComponent> results = new ArrayList<>();

        for (DocComponent component : components) {
            results.add(componentFactory.createCustomComponent(component));
        }

        return results;
    }
}
