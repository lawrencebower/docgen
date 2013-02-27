package org.lawrencebower.docgen.core.generator.utils;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static junit.framework.Assert.assertEquals;
import static junit.framework.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class PDFConcatenatorTest extends AbstractIntegrationTest {

    @Autowired
    PDFConcatenator pdfConcatenator;

    @Autowired
    DocGenFileUtils fileUtils;

    private File outputFile;
    private File inputFile1;
    private File inputFile2;

    private static final String outputFileName = "concatenated_files.pdf";
    private static final String inputFile1Name = "concat_input_1.pdf";
    private static final String inputFile2Name = "concat_input_2.pdf";
    private static final String expectedOut_1_file_Name = "1_concatenated_file_expected_output.pdf";
    private static final String expectedOut_2_files_Name = "2_concatenated_files_expected_output.pdf";
    private static final String expectedOut_3_files_Name = "1_concatenated_files_multiple_copies_expected_output.pdf";
    private static final String expectedOut_4_files_Name = "2_concatenated_files_multiple_copies_expected_output.pdf";

    @Before
    public void setUp() {
        prepareDirs();
        this.outputFile = new File(outputPackage + outputFileName);
        this.inputFile1 = classPathResourceToFile(inputPackage + inputFile1Name);
        this.inputFile2 = classPathResourceToFile(inputPackage + inputFile2Name);
    }

    @Test
    public void testConcatenatePDFs_sourceFilesDontExist_throwsError() throws Exception {
        try {
            File file = new File("i dont exist");
            List<PDFDocument> sourceFiles = mockPDFList(file);
            pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);
        } catch (DocGenException e) {
            assertEquals("file does not exist 'i dont exist'", e.getMessage());
        }
    }

    private List<PDFDocument> mockPDFList(File... files) {

        List<PDFDocument> results = new ArrayList<>();

        for (File file : files) {
            PDFDocument pdf = mock(PDFDocument.class);
            when(pdf.getFile()).thenReturn(file);
            when(pdf.getCopyNumber()).thenReturn(1);
            results.add(pdf);
        }

        return results;
    }

    @Test
    public void testConcatenatePDFs_outputFileDoesntExist_makesFile() throws Exception {

        List<PDFDocument> sourceFiles = mockPDFList(inputFile1, inputFile2);

        File nonExistantOutput = new File(outputPackage + "non_existant_output.pdf");
        fileUtils.deleteQuietly(nonExistantOutput);

        pdfConcatenator.concatenatePDFs(sourceFiles, nonExistantOutput);

        assertTrue(nonExistantOutput.exists());
    }

    @Test
    public void testConcatenatePDFs_outputFileExists_noError() throws Exception {

        List<PDFDocument> sourceFiles = mockPDFList(inputFile1, inputFile2);

        File nonExistantOutput = new File(outputPackage + "non_existant_output.pdf");
        nonExistantOutput.createNewFile();

        pdfConcatenator.concatenatePDFs(sourceFiles, nonExistantOutput);

        assertTrue(nonExistantOutput.exists());
    }

    @Test
    public void testConcatenatePDFs_twoSourceFiles_concatenatesOkay() throws Exception {

        List<PDFDocument> sourceFiles = mockPDFList(inputFile1, inputFile2);

        pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);

        File expectedOutputFile = classPathResourceToFile(inputPackage + expectedOut_2_files_Name);

        boolean filesAreSame = checksumUtils.filteredFileChecksumsAreSame(expectedOutputFile, outputFile);

        assertTrue(filesAreSame);
    }

    @Test
    public void testConcatenatePDFs_oneSourceFile_concatenatesOkay() throws Exception {

        List<PDFDocument> sourceFiles = mockPDFList(inputFile1);

        pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);

        File expectedOutputFile = classPathResourceToFile(inputPackage + expectedOut_1_file_Name);

        boolean filesAreSame = checksumUtils.filteredFileChecksumsAreSame(expectedOutputFile, outputFile);

        assertTrue(filesAreSame);
    }

    @Test
    public void testConcatenatePDFs_multipleCopiesOfOneFile_concatenatesOkay() throws Exception {

        PDFDocument pdf = mock(PDFDocument.class);
        when(pdf.getFile()).thenReturn(inputFile1);
        when(pdf.getCopyNumber()).thenReturn(3);

        List<PDFDocument> sourceFiles = Arrays.asList(pdf);

        pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);

        File expectedOutputFile = classPathResourceToFile(inputPackage + expectedOut_3_files_Name);

        boolean filesAreSame = checksumUtils.filteredFileChecksumsAreSame(expectedOutputFile, outputFile);

        assertTrue(filesAreSame);
    }

    @Test
    public void testConcatenatePDFs_multipleCopiesMixedFiles_concatenatesOkay() throws Exception {

        PDFDocument pdf1 = mock(PDFDocument.class);
        when(pdf1.getFile()).thenReturn(inputFile1);
        when(pdf1.getCopyNumber()).thenReturn(2);

        PDFDocument pdf2 = mock(PDFDocument.class);
        when(pdf2.getFile()).thenReturn(inputFile2);
        when(pdf2.getCopyNumber()).thenReturn(1);

        List<PDFDocument> sourceFiles = Arrays.asList(pdf1, pdf2);

        pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);

        File expectedOutputFile = classPathResourceToFile(inputPackage + expectedOut_4_files_Name);

        boolean filesAreSame = checksumUtils.filteredFileChecksumsAreSame(expectedOutputFile, outputFile);

        assertTrue(filesAreSame);
    }

}
