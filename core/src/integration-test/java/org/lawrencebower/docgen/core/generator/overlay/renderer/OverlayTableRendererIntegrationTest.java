package org.lawrencebower.docgen.core.generator.overlay.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.DocCoordinates;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.model.itext_component.utils.LayoutTableGenerator;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class OverlayTableRendererIntegrationTest extends AbstractOverlayRendererTest {

    /**
     * read from classpath
     */
    Resource inputFilePath = new ClassPathResource("/org/lawrencebower/docgen/core/generator/overlay/renderer/table_renderer_input.pdf");

    @Before
    public void setup() {
        prepareDirs();
    }

    @Test
    public void testRenderComponent_textAlignment_IsValid() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output.pdf";
        String outFilePath = outputPackage + "table_renderer_output.pdf";

        int width = 100;
        int height = 200;

        DocCoordinates leftCoordinates = new DocCoordinates(10, 545, width, height);
        TableComponent tableComponent1 = getTableComponent(leftCoordinates);
        tableComponent1.setRenderBorder(true);

        DocCoordinates rightCoordinates = new DocCoordinates(150, 445, width, height);
        TableComponent tableComponent2 = getTableComponent(rightCoordinates);
        tableComponent2.setRenderBorder(true);

        DocCoordinates centerCoordinates = new DocCoordinates(300, 345, width, height);
        TableComponent tableComponent3 = getTableComponent(centerCoordinates);
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

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_2.pdf";
        String outFilePath = outputPackage + "table_renderer_output_2.pdf";

        int width = 400;
        int height = 200;

        DocCoordinates tableCoordinates = new DocCoordinates(100, 345, width, height);

        TableComponent tableComponent = getTableComponent(tableCoordinates);

        tableComponent.setRenderBorder(true);

        tableComponent.setName("main table");

        List<TableCell> allCells = tableComponent.getAllRenderableCells();

        TableComponent nestedTableComponent = LayoutTableGenerator.makeLayoutTableComponent(3, 3);
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

    public TableComponent getTableComponent(DocCoordinates coordinates) {
        TableComponent tableComponent = LayoutTableGenerator.makeLayoutTableComponent(3, 3);
        tableComponent.setCoordinates(coordinates);
        return tableComponent;
    }

}
