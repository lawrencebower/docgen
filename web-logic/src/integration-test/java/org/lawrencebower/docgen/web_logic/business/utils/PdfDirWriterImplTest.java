package org.lawrencebower.docgen.web_logic.business.utils;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;

import static junit.framework.Assert.assertTrue;
import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-integration-test-context.xml")
public class PdfDirWriterImplTest {

    /**
     * rather than injecting this with spring, manually create a "real" pdfDirWriter,
     * rather than use the test version that is injected for all other tests
     */
    PdfDirWriterImpl pdfDirWriter;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String outputRoot;

    @Before
    public void setUp() {
        pdfDirWriter = new PdfDirWriterImpl();
    }

    @Test(expected = DocGenException.class)
    public void testCreatePDFDir_invalidRoot_throwsError() throws Exception {
        try {
            pdfDirWriter.createPDFDir("I dont exist");
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals("Directory root 'I dont exist' does not exist?!", message);
            throw e;
        }
    }

    @Test
    public void testCreatePDFDir_validRoot_createsDir() throws Exception {
        File result = pdfDirWriter.createPDFDir(outputRoot);
        result.deleteOnExit();//to tidy up afterwards
        boolean dirCreated = result.exists();
        assertTrue(dirCreated);
    }

}
