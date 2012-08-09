package org.lawrencebower.docgen.core.generator;

import org.lawrencebower.docgen.core.document.OverlayDocumentInfo;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.type.DocType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFGenUtilsImpl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class PDFGenUtilsImplTest {

    private PDFGenUtilsImpl pdfGenUtils;
    private OverlayDocumentInfo docInfo;

    @org.junit.Before
    public void setUp() throws Exception {
        this.pdfGenUtils = new PDFGenUtilsImpl();
        this.docInfo = mock(OverlayDocumentInfo.class);
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_noDocType_throwsError() throws Exception {


        when(docInfo.getDocType()).thenReturn(null);
        List<DocComponent> textComponents = Arrays.asList(mock(DocComponent.class));
        when(docInfo.getComponents()).thenReturn(textComponents);
        when(docInfo.getName()).thenReturn("name");

        try {
            pdfGenUtils.checkRequiredValuesPresent(docInfo);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("DocInfo DocType must not be null"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_emptyDocComponents_throwsError() throws Exception {

        OverlayDocumentInfo testDoc = mock(OverlayDocumentInfo.class);

        when(testDoc.getDocType()).thenReturn(DocType.OVERLAY);
        when(testDoc.getComponents()).thenReturn(new ArrayList<DocComponent>());
        when(testDoc.getName()).thenReturn("name");

        try {
            pdfGenUtils.checkRequiredValuesPresent(testDoc);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("DocInfo Document components are null/empty"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_nullDocComponents_throwsError() throws Exception {

        OverlayDocumentInfo testDoc = mock(OverlayDocumentInfo.class);

        when(testDoc.getDocType()).thenReturn(DocType.OVERLAY);
        when(testDoc.getComponents()).thenReturn(null);
        when(testDoc.getName()).thenReturn("name");

        try {
            pdfGenUtils.checkRequiredValuesPresent(testDoc);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("DocInfo Document components are null/empty"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckRequiredValuesPresent_nullName_throwsError() throws Exception {

        OverlayDocumentInfo testDoc = mock(OverlayDocumentInfo.class);

        when(testDoc.getDocType()).thenReturn(DocType.OVERLAY);
        List<DocComponent> textComponents = Arrays.asList(mock(DocComponent.class));
        when(testDoc.getComponents()).thenReturn(textComponents);
        when(testDoc.getName()).thenReturn(null);

        try {
            pdfGenUtils.checkRequiredValuesPresent(testDoc);
        } catch (DocGenException e) {
            assertTrue(e.getMessage().equals("DocInfo Name must not be null"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckCoordinates_validCoordinates_noError() {

        DocPosition position = new DocPosition(DocAlignment.LEFT, new DocCoordinates(1, 1, 1, 1));
        DocComponent component = new TextComponent("name", position, "value");
        pdfGenUtils.checkCoordinates(Arrays.asList(component));
    }

    @org.junit.Test
    public void testCheckCoordinates_nullPosition_throwsError() {

        try {
            DocComponent component = new TextComponent("name", null, "value");
            pdfGenUtils.checkCoordinates(Arrays.asList(component));
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertTrue(message.startsWith("Position is null"));
            return;
        }
        fail();//should not get here
    }

    @org.junit.Test
    public void testCheckCoordinates_nullCoordinate_throwsError() {

        try {
            DocPosition position = new DocPosition(DocAlignment.LEFT, null);
            DocComponent component = new TextComponent("name", position, "value");
            pdfGenUtils.checkCoordinates(Arrays.asList(component));
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertTrue(message.startsWith("Coordinates are null"));
            return;
        }
        fail();//should not get here
    }

}
