package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class CustomTableRendererIntegrationTest extends AbstractCustomRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_validComponent_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "table_renderer_output.pdf";

        TableComponent tableComponent = new TableComponent("Table Name");

        tableComponent.setHeaderRow(new TableCell("col 1"),
                                    new TableCell("col 2"),
                                    new TableCell("col 3"));

        tableComponent.addRow(getTableRow());
        tableComponent.addRow(getTableRow());
        tableComponent.addRow(getTableRow());


        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    public TableRow getTableRow() {
        TableRow tableRow = new TableRow();
        tableRow.addCell(new TableCell("1"));
        tableRow.addCell(new TableCell("2"));
        tableRow.addCell(new TableCell("3"));
        return tableRow;
    }
}
