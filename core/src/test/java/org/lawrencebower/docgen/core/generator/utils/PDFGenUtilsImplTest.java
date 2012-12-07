package org.lawrencebower.docgen.core.generator.utils;

import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.DocType;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.ITextComponentFactory;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/core-test-context.xml"})
public class PDFGenUtilsImplTest {

    @Autowired
    private ITextComponentFactory componentFactory;

    private PDFGenUtilsImpl pdfGenUtils;
    private OverlayDocument document;

    @org.junit.Before
    public void setUp() throws Exception {
        this.pdfGenUtils = new PDFGenUtilsImpl();
        this.document = mock(OverlayDocument.class);
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_noDocType_throwsError() throws Exception {


        when(document.getDocType()).thenReturn(null);
        List<OverlayComponent> textComponents = Arrays.asList(mock(OverlayComponent.class));
        when(document.getComponents()).thenReturn(textComponents);
        when(document.getName()).thenReturn("name");

        try {
            pdfGenUtils.checkRequiredValuesPresent(document);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("Document DocType must not be null"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_emptyDocComponents_throwsError() throws Exception {

        OverlayDocument testDoc = mock(OverlayDocument.class);

        when(testDoc.getDocType()).thenReturn(DocType.OVERLAY);
        when(testDoc.getComponents()).thenReturn(new ArrayList<OverlayComponent>());
        when(testDoc.getName()).thenReturn("name");

        try {
            pdfGenUtils.checkRequiredValuesPresent(testDoc);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("Document Document components are null/empty"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_nullDocComponents_throwsError() throws Exception {

        OverlayDocument testDoc = mock(OverlayDocument.class);

        when(testDoc.getDocType()).thenReturn(DocType.OVERLAY);
        when(testDoc.getComponents()).thenReturn(null);
        when(testDoc.getName()).thenReturn("name");

        try {
            pdfGenUtils.checkRequiredValuesPresent(testDoc);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("Document Document components are null/empty"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_nullName_throwsError() throws Exception {

        OverlayDocument testDoc = mock(OverlayDocument.class);

        when(testDoc.getDocType()).thenReturn(DocType.OVERLAY);
        List<OverlayComponent> textComponents = Arrays.asList(mock(OverlayComponent.class));
        when(testDoc.getComponents()).thenReturn(textComponents);
        when(testDoc.getName()).thenReturn(null);

        try {
            pdfGenUtils.checkRequiredValuesPresent(testDoc);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("Document Name must not be null"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckCoordinates_validCoordinates_noError() {

        TextComponent component = new TextComponent(HorizontalAlignment.LEFT, "value");
        component.setCoordinates(new DocCoordinates(1, 1, 1, 1));
        ITextComponent overlayText = componentFactory.createTextComponent(component);
        pdfGenUtils.checkCoordinatesPresent(Arrays.asList(overlayText));
    }

    @org.junit.Test
    public void testCheckCoordinates_nullCoordinate_throwsError() {

        try {
            TextComponent component = new TextComponent(HorizontalAlignment.LEFT, "value");
            ITextComponent overlayText = componentFactory.createTextComponent(component);
            pdfGenUtils.checkCoordinatesPresent(Arrays.asList(overlayText));
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertTrue(message.startsWith("Coordinates are null"));
            return;
        }
        fail();//should not get here
    }

    public void testGetPDFReaderAndUnlockForSourcePDF_lockedPdf_isUnlocked(){
        //todo
    }

}
