package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.utils.ITextTableGeneratorTest;
import org.lawrencebower.docgen.core.generator.utils.TextGenerator;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.awt.*;
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
    public void testRenderComponent_alignedCellContents_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_2.pdf";
        String outFilePath = outputPackage + "table_renderer_output_2.pdf";

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(4, 2);
        List<TableCell> allCells = tableComponent.getAllCells();

        allCells.get(0).setText("\n\n\n\n");//make the cell deep

        allCells.get(1).setVerticalAlignment(VerticalAlignment.TOP);
        allCells.get(2).setVerticalAlignment(VerticalAlignment.MIDDLE);
        allCells.get(3).setVerticalAlignment(VerticalAlignment.BOTTOM);

        allCells.get(4).setText("\n\n\n\n");//make the cell deep

        allCells.get(5).getComponent().getPosition().setHorizontalAlignment(HorizontalAlignment.LEFT);
        allCells.get(6).getComponent().getPosition().setHorizontalAlignment(HorizontalAlignment.CENTER);
        allCells.get(7).getComponent().getPosition().setHorizontalAlignment(HorizontalAlignment.RIGHT);

        allCells.get(8).setText("\n\n\n\n");//make the cell deep

        allCells.get(9).getComponent().getPosition().setHorizontalAlignment(HorizontalAlignment.LEFT);
        allCells.get(9).setVerticalAlignment(VerticalAlignment.BOTTOM);
        allCells.get(10).getComponent().getPosition().setHorizontalAlignment(HorizontalAlignment.CENTER);
        allCells.get(10).setVerticalAlignment(VerticalAlignment.MIDDLE);
        allCells.get(11).getComponent().getPosition().setHorizontalAlignment(HorizontalAlignment.RIGHT);
        allCells.get(11).setVerticalAlignment(VerticalAlignment.TOP);

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

    @Test
    public void testRenderComponent_cellBackgroundColor_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_4.pdf";
        String outFilePath = outputPackage + "table_renderer_output_4.pdf";

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        List<TableCell> allCells = tableComponent.getAllCells();
        allCells.get(0).setBackgroundColor(Color.cyan);
        allCells.get(2).setBackgroundColor(Color.pink);
        allCells.get(4).setBackgroundColor(Color.gray);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_cellPadding_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_5.pdf";
        String outFilePath = outputPackage + "table_renderer_output_5.pdf";

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        List<TableCell> allCells = tableComponent.getAllCells();
        allCells.get(0).setPadding(0);
        allCells.get(4).setPadding(20);
        allCells.get(8).setPadding(50);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_variedFonts_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_6.pdf";
        String outFilePath = outputPackage + "table_renderer_output_6.pdf";

        List<TextBlock> textBlocks = TextGenerator.createVariedTextBlocks();
        TextBlock variedTxtBlock = TextGenerator.createVariedTextBlock();

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        List<TableCell> allCells = tableComponent.getAllCells();
        allCells.get(0).setComponent(new TextComponent(textBlocks.get(0)));
        allCells.get(4).setComponent(new TextComponent(textBlocks.get(1)));
        allCells.get(8).setComponent(new TextComponent(textBlocks.get(2)));
        allCells.get(9).setComponent(new TextComponent(variedTxtBlock));

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_noBorder_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_7.pdf";
        String outFilePath = outputPackage + "table_renderer_output_7.pdf";

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
        tableComponent.setRenderBorder(false);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_mixedCellContent_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_8.pdf";
        String outFilePath = outputPackage + "table_renderer_output_8.pdf";

        TableComponent tableComponent = ITextTableGeneratorTest.makeStandardTableComponent(3, 3);
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
                                        tableComponent);

    }

}
