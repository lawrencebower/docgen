package org.lawrencebower.docgen.core.generator.utils;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
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

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
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

    @Before
    public void setUp() {
        super.prepareDirs();
        this.outputFile = new File(outputPackage + outputFileName);
        this.inputFile1 = new File(inputPackage + inputFile1Name);
        this.inputFile2 = new File(inputPackage + inputFile2Name);
    }

    @Test
    public void testConcatenatePDFs_sourceFilesDontExist_throwsError() throws Exception {
        try {
            List<File> sourceFiles = new ArrayList<>();
            sourceFiles.add(new File("i dont exist"));
            pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);
        } catch (DocGenException e) {
            assertEquals("file does not exist 'i dont exist'", e.getMessage());
        }
    }

    @Test
    public void testConcatenatePDFs_outputFileDoesntExist_makesFile() throws Exception {
        List<File> sourceFiles = Arrays.asList(inputFile1, inputFile2);

        File nonExistantOutput = new File(outputPackage + "non_existant_output.pdf");
        fileUtils.deleteFileIfAlreadyExists(nonExistantOutput);

        pdfConcatenator.concatenatePDFs(sourceFiles, nonExistantOutput);

        assertTrue(nonExistantOutput.exists());
    }

    @Test
    public void testConcatenatePDFs_outputFileExists_noError() throws Exception {
        List<File> sourceFiles = Arrays.asList(inputFile1, inputFile2);

        File nonExistantOutput = new File(outputPackage + "non_existant_output.pdf");
        nonExistantOutput.createNewFile();

        pdfConcatenator.concatenatePDFs(sourceFiles, nonExistantOutput);

        assertTrue(nonExistantOutput.exists());
    }

    @Test
    public void testConcatenatePDFs_twoSourceFiles_concatenatesOkay() throws Exception {
        List<File> sourceFiles = Arrays.asList(inputFile1, inputFile2);

        pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);

        File expectedOutputFile = new File(inputPackage + expectedOut_2_files_Name);

        boolean filesAreSame = checksumUtils.filteredFileChecksumsAreSame(expectedOutputFile, outputFile);

        assertTrue(filesAreSame);
    }

    @Test
    public void testConcatenatePDFs_oneSourceFile_concatenatesOkay() throws Exception {
        List<File> sourceFiles = Arrays.asList(inputFile1);

        pdfConcatenator.concatenatePDFs(sourceFiles, outputFile);

        File expectedOutputFile = new File(inputPackage + expectedOut_1_file_Name);

        boolean filesAreSame = checksumUtils.filteredFileChecksumsAreSame(expectedOutputFile, outputFile);

        assertTrue(filesAreSame);
    }

}
