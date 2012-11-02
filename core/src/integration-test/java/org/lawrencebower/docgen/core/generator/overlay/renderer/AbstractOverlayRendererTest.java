package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocumentInfo;
import org.lawrencebower.docgen.core.document.component.CheckBoxComponent;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.ImageComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.generator.overlay.OverlayPDFGenerator;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponentFactory;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayTextComponent;
import org.springframework.beans.factory.annotation.Autowired;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertTrue;

public abstract class AbstractOverlayRendererTest extends AbstractIntegrationTest {

    @Autowired
    private OverlayPDFGenerator pdfGenerator;
    @Autowired
    private OverlayComponentFactory componentFactory;

    @Autowired
    private OverlayPDFGenerator overlayGenerator;

    protected void createPDFAndCompareWithExpected(String expectedOutputFilePath,
                                                   String outFilePath,
                                                   String sourcePDF,
                                                   DocComponent... components) {

        OverlayDocumentInfo docInfo = new OverlayDocumentInfo("Doc name", pdfGenerator);

        docInfo.setSourcePDF(sourcePDF);

        List<OverlayComponent> overlayComponents = convertComponents(components);

        docInfo.setComponents(overlayComponents);

        PDFDocument pdfDocument = overlayGenerator.generatePDF(docInfo);

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
         switch (component.getComponentType()){
             case TEXT: return createOverlayText((TextComponent) component);
             case TABLE: return createOverlayTable((TableComponent) component);
             case IMAGE: return createOverlayImage((ImageComponent) component);
             case CHECKBOX: return createOverlayCheckBox((CheckBoxComponent) component);
         }
        throw new DocGenException("DocComponent not mapped to OverlayComponent? " + component.getClass());
    }

    private OverlayComponent createOverlayCheckBox(CheckBoxComponent component) {
        return componentFactory.createOverlayCheckBox(component);
    }

    private OverlayComponent createOverlayImage(ImageComponent component) {
        return componentFactory.createOverlayImage(component);
    }

    private OverlayComponent createOverlayTable(TableComponent component) {
        return componentFactory.createOverlayTable(component);
    }

    private OverlayTextComponent createOverlayText(TextComponent component) {
        return componentFactory.createOverlayText(component);
    }
}
