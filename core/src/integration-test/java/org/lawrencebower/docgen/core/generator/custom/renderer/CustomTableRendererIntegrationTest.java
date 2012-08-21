package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.position.DocAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.utils.ITextTableGeneratorTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/integration-test-config.xml"})
public class CustomTableRendererIntegrationTest extends AbstractCustomRendererTest {

    @Before
    public void setup() {
        super.prepareDirs();
    }

    @Test
    public void testRenderComponent_validComponent_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_1.pdf";
        String outFilePath = outputPackage + "table_renderer_output_1.pdf";

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_alignedCells_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_2.pdf";
        String outFilePath = outputPackage + "table_renderer_output_2.pdf";

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(4, 2);
        List<TableCell> allCells = tableComponent.getAllCells();

        allCells.get(0).setValue("\n\n\n\n");//make the cell deep

        allCells.get(1).setVerticalAlignment(DocAlignment.TOP);
        allCells.get(2).setVerticalAlignment(DocAlignment.MIDDLE);
        allCells.get(3).setVerticalAlignment(DocAlignment.BOTTOM);

        allCells.get(4).setValue("\n\n\n\n");//make the cell deep

        allCells.get(5).setHorizontalAlignment(DocAlignment.LEFT);
        allCells.get(6).setHorizontalAlignment(DocAlignment.CENTER);
        allCells.get(7).setHorizontalAlignment(DocAlignment.RIGHT);

        allCells.get(8).setValue("\n\n\n\n");//make the cell deep

        allCells.get(9).setHorizontalAlignment(DocAlignment.LEFT);
        allCells.get(9).setVerticalAlignment(DocAlignment.BOTTOM);
        allCells.get(10).setHorizontalAlignment(DocAlignment.CENTER);
        allCells.get(10).setVerticalAlignment(DocAlignment.MIDDLE);
        allCells.get(11).setHorizontalAlignment(DocAlignment.RIGHT);
        allCells.get(11).setVerticalAlignment(DocAlignment.TOP);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_columnWidthsSet_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_3.pdf";
        String outFilePath = outputPackage + "table_renderer_output_3.pdf";

        TableComponent tableComponent1 = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        tableComponent1.getHeaderRow().setColumnWidths(20, 20, 60);

        TableComponent tableComponent2 = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        tableComponent2.getHeaderRow().setColumnWidths(60, 10, 30);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent1,
                                        new NewLineComponent(),
                                        tableComponent2);

    }

}
