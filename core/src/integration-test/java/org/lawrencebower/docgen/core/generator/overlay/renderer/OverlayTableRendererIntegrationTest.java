package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.position.DocPosition;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.utils.ITextTableGeneratorTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

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
        DocPosition leftPosition = new DocPosition(leftCoordinates);
        TableComponent tableComponent1 = getTableComponent(leftPosition);
        tableComponent1.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        DocPosition rightPosition = new DocPosition(rightCoordinates);
        TableComponent tableComponent2 = getTableComponent(rightPosition);
        tableComponent2.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        DocPosition centerPosition = new DocPosition(centerCoordinates);
        TableComponent tableComponent3 = getTableComponent(centerPosition);
        tableComponent3.setRenderBorder(true);
        tableComponent3.setWidthPercentage(50);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        tableComponent1,
                                        tableComponent2,
                                        tableComponent3);

    }

    @Test
    public void testRenderComponent_mixedCellContent_createsValidFile() {

        String inputFilePath = inputPackage + "table_renderer_input.pdf";
        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_2.pdf";
        String outFilePath = outputPackage + "table_renderer_output_2.pdf";

        int width = 400;
        int height = 200;

        DocCoordinates tableCoordinates = new DocCoordinates(100, 345, width, height);
        DocPosition tablePosition = new DocPosition(tableCoordinates);

        TableComponent tableComponent = getTableComponent(tablePosition);

        tableComponent.setRenderBorder(true);

        tableComponent.setName("main table");

        List<TableCell> allCells = tableComponent.getAllCells();

        TableComponent nestedTableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        nestedTableComponent.setName("nested table");
        nestedTableComponent.setWidthPercentage(100);
        TableCell tableCell = allCells.get(3);
        tableCell.setPadding(0);
        tableCell.setComponent(nestedTableComponent);

        TextComponent nestedTextComponent = new TextComponent("This is a text component");
        allCells.get(4).setComponent(nestedTextComponent);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        inputFilePath,
                                        tableComponent);

    }

    public TableComponent getTableComponent(DocPosition position) {
        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        tableComponent.setPosition(position);
        return tableComponent;
    }

}
