package org.lawrencebower.docgen.core.generator.utils;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.DocType;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.overlay.OverlayDocument;
import org.lawrencebower.docgen.core.generator.overlay.component.OverlayComponent;
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
@ContextConfiguration(locations = "classpath:META-INF/core-test-context.xml")
public class PDFGenUtilsImplTest {

    private PDFGenUtilsImpl pdfGenUtils;
    private OverlayDocument document;

    @Before
    public void setUp() throws Exception {
        pdfGenUtils = new PDFGenUtilsImpl();
        document = mock(OverlayDocument.class);
    }

    @Test
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

    @Test
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

    @Test
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

    @Test
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

    public void testGetPDFReaderAndUnlockForSourcePDF_lockedPdf_isUnlocked(){
        //todo
    }

}
