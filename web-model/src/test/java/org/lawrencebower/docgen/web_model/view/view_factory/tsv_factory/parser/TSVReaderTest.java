package org.lawrencebower.docgen.web_model.view.view_factory.tsv_factory.parser;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.ByteArrayInputStream;
import java.util.List;

import static junit.framework.TestCase.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-model-test-context.xml")
public class TSVReaderTest {

    @Autowired
    TSVReader tsvReader;

    @Test
    public void testReadTsv_quotedTokens_tokensStripped(){
        String input = "\"#hello\"\t\"I am comment line\"\n" +
                       "\"I am first\"\t\"proper line\"";
        byte[] bytes = input.getBytes();
        DataSet dataSet = tsvReader.readTSV(new ByteArrayInputStream(bytes));

        List<DataRow> rows = dataSet.getRows();
        assertEquals(1, rows.size());
        DataRow dataRow = rows.get(0);
        assertEquals("I am first", dataRow.getString(0));
        assertEquals("proper line", dataRow.getString(1));
    }
}
