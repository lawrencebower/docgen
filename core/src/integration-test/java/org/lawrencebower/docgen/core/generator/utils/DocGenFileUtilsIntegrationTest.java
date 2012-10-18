package org.lawrencebower.docgen.core.generator.utils;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.AbstractIntegrationTest;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.io.IOException;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class DocGenFileUtilsIntegrationTest extends AbstractIntegrationTest {

    @Autowired
    DocGenFileUtils fileUtils;

    @Autowired
    ChecksumUtils checksumUtils;

    String exampleFileString = "example_file.pdf";

    @Before
    public void setup(){
        super.prepareDirs();
    }

    @Test
    public void testDeleteFileIfAlreadyExists_fileExists_fileDeleted() throws Exception {
        File file = createExampleFileAndCheckExists();
        fileUtils.deleteFileIfAlreadyExists(file);
        boolean fileExists = file.exists();
        assertFalse(fileExists);
    }

    @Test
    public void testDeleteFileIfAlreadyExists_fileDoesNotExist_noError() throws Exception {
        File file = new File("I was never created");
        fileUtils.deleteFileIfAlreadyExists(file);
        boolean fileExists = file.exists();
        assertFalse(fileExists);
    }

    @Test
    public void testGetChecksum_exampleFile_returnsExpectedChecksum() throws IOException {
        File exampleFile = createExampleFileAndCheckExists();
        String checksum = checksumUtils.getChecksumFromFile(exampleFile);
        assertEquals("d41d8cd98f00b204e9800998ecf8427e", checksum);
    }

    private File createExampleFileAndCheckExists() throws IOException {
        File exampleFile = new File(outputPackage + exampleFileString);
        exampleFile.createNewFile();
        if(!exampleFile.exists()){
            throw new DocGenException("Problem making example file");
        }
        return exampleFile;
    }
}
