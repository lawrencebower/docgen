package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.TableRow;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class OverlayTableRendererIntegrationTest extends AbstractOverlayRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_textAlignment_IsValid() {

        String inputFilePath = inputPackage + "table_renderer_input.pdf";
        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "table_renderer_output.pdf";

        int width = 100;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        DocPosition leftPosition = new DocPosition(DocAlignment.LEFT, leftCoordinates);
        TableComponent leftTextComponent = getTableComponent(leftPosition);
        leftTextComponent.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        DocPosition rightPosition = new DocPosition(DocAlignment.RIGHT, rightCoordinates);
        TableComponent rightTextComponent = getTableComponent(rightPosition);
        rightTextComponent.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        DocPosition centerPosition = new DocPosition(DocAlignment.CENTER, centerCoordinates);
        TableComponent centerTextComponent = getTableComponent(centerPosition);
        centerTextComponent.setRenderBorder(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        leftTextComponent,
                                        rightTextComponent,
                                        centerTextComponent);

    }

    public TableComponent getTableComponent(DocPosition position) {
        TableComponent tableComponent = new TableComponent("Table Name", position);

        tableComponent.setHeaderRow(new TableCell("col 1"),
                                    new TableCell("col 2"),
                                    new TableCell("col 3"));

        tableComponent.addRow(getTableRow());
        tableComponent.addRow(getTableRow());
        tableComponent.addRow(getTableRow());

        return tableComponent;
    }

    public TableRow getTableRow() {
        TableRow tableRow = new TableRow();
        tableRow.addCell(new TableCell("1"));
        tableRow.addCell(new TableCell("2"));
        tableRow.addCell(new TableCell("3"));
        return tableRow;
    }
}
