package org.lawrencebower.docgen.core.generator.custom.renderer;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.position.VerticalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableCell;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.view_table.*;
import org.lawrencebower.docgen.core.document.component.text.TextBlock;
import org.lawrencebower.docgen.core.generator.model.itext_component.utils.WebTableGenerator;
import org.lawrencebower.docgen.core.generator.utils.TextGenerator;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.awt.*;
import java.util.List;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/integration-test-config.xml")
public class CustomTableRendererIntegrationTest extends AbstractCustomRendererTest {

    @Before
    public void setup() {
        prepareDirs();
    }

    @Test
    public void testRenderComponent_validComponent_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_1.pdf";
        String outFilePath = outputPackage + "table_renderer_output_1.pdf";

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_alignedCellContents_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_2.pdf";
        String outFilePath = outputPackage + "table_renderer_output_2.pdf";

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(4, 2);
        List<TableCell> allCells = tableComponent.getAllRenderableCells();

        allCells.get(0).setComponent(new TableTextComponent("\n\n\n\n"));//make the cell deep

        allCells.get(1).setVerticalAlignment(VerticalAlignment.TOP);
        allCells.get(2).setVerticalAlignment(VerticalAlignment.MIDDLE);
        allCells.get(3).setVerticalAlignment(VerticalAlignment.BOTTOM);

        allCells.get(4).setComponent(new TableTextComponent("\n\n\n\n"));//make the cell deep

        TableTextComponent leftAlignedComponent = new TableTextComponent("1");
        leftAlignedComponent.setAlignment(HorizontalAlignment.LEFT);
        allCells.get(5).setComponent(leftAlignedComponent);

        TableTextComponent centerAlignedComponent = new TableTextComponent(HorizontalAlignment.CENTER);
        centerAlignedComponent.setText("2");
        allCells.get(6).setComponent(centerAlignedComponent);

        TableTextComponent rightAlignedComponent = new TableTextComponent(HorizontalAlignment.RIGHT,
                                                                          "3");
        allCells.get(7).setComponent(rightAlignedComponent);

        allCells.get(8).setComponent(new TableTextComponent("\n\n\n\n"));//make the cell deep

        allCells.get(9).getComponent().setAlignment(HorizontalAlignment.LEFT);
        allCells.get(9).setVerticalAlignment(VerticalAlignment.BOTTOM);
        allCells.get(10).getComponent().setAlignment(HorizontalAlignment.CENTER);
        allCells.get(10).setVerticalAlignment(VerticalAlignment.MIDDLE);
        allCells.get(11).getComponent().setAlignment(HorizontalAlignment.RIGHT);
        allCells.get(11).setVerticalAlignment(VerticalAlignment.TOP);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_columnWidthsSet_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_3.pdf";
        String outFilePath = outputPackage + "table_renderer_output_3.pdf";

        ViewTableComponent tableComponent1 = WebTableGenerator.makeWebTableComponent(3, 3);
        WebTableHeaderRow headerRow = tableComponent1.getHeaderRow();
        headerRow.getCells().clear();
        headerRow.addCell(new ViewHeaderCell("col0", 20));
        headerRow.addCell(new ViewHeaderCell("col1", 20));
        headerRow.addCell(new ViewHeaderCell("col2", 60));

        TableComponent tableComponent2 = WebTableGenerator.makeWebTableComponent(3, 3);
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

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        List<TableCell> allCells = tableComponent.getAllRenderableCells();
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

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        List<TableCell> allCells = tableComponent.getAllRenderableCells();
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

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        List<TableCell> allCells = tableComponent.getAllRenderableCells();
        allCells.get(0).setComponent(new TableTextComponent(textBlocks.get(0)));
        allCells.get(4).setComponent(new TableTextComponent(textBlocks.get(1)));
        allCells.get(8).setComponent(new TableTextComponent(textBlocks.get(2)));
        allCells.get(9).setComponent(new TableTextComponent(variedTxtBlock));

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_noBorder_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_7.pdf";
        String outFilePath = outputPackage + "table_renderer_output_7.pdf";

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        tableComponent.setRenderBorder(false);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_mixedCellContent_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_8.pdf";
        String outFilePath = outputPackage + "table_renderer_output_8.pdf";

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        tableComponent.setName("main table");

        List<TableCell> allCells = tableComponent.getAllRenderableCells();

        TableComponent nestedTableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        nestedTableComponent.setName("nested table");
        nestedTableComponent.setWidthPercentage(100);
        TableCell tableCell = allCells.get(3);
        tableCell.setPadding(0);
        tableCell.setComponent(nestedTableComponent);

        TextComponent nestedTextComponent = new TextComponent("This is a TextComponent, not a TableTextComponent");
        allCells.get(4).setComponent(nestedTextComponent);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_mixedTableAndCellPadding_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_9.pdf";
        String outFilePath = outputPackage + "table_renderer_output_9.pdf";

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        tableComponent.setName("main table");
        tableComponent.setTablePadding(5);

        List<TableCell> allCells = tableComponent.getAllRenderableCells();

        allCells.get(3).setPadding(0);

        allCells.get(7).setPadding(10);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

    @Test
    public void testRenderComponent_renderHeaderFalse_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_10.pdf";
        String outFilePath = outputPackage + "table_renderer_output_10.pdf";

        TableComponent tableComponent = WebTableGenerator.makeWebTableComponent(3, 3);
        tableComponent.setName("main table");
        tableComponent.getHeaderRow().setRenderHeader(false);

        TableComponent tableComponent2 = WebTableGenerator.makeWebTableComponent(3, 3);
        tableComponent2.setName("main table 2");
        tableComponent2.getHeaderRow().setRenderHeader(true);

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent,
                                        tableComponent2);

    }

    @Test
    public void testRenderComponent_colAndRowSpan_createsValidFile() {

        String expectedOutputFilePath = inputPackage + "table_renderer_expected_output_11.pdf";
        String outFilePath = outputPackage + "table_renderer_output_11.pdf";

        ViewTableComponent tableComponent = new ViewTableComponent("table");
        tableComponent.setRenderBorder(true);

        tableComponent.setHeaderRow(new ViewHeaderCell("col1"),
                                    new ViewHeaderCell("col2"),
                                    new ViewHeaderCell("col3"));


        ViewTableRow row1 = new ViewTableRow("row1");

        row1.addCell(new ViewTableCell("cel1"));
        ViewTableCell cell2 = new ViewTableCell("colspan 2");
        cell2.setColSpan(2);

        row1.addCell(cell2);
        tableComponent.addRow(row1);

        ViewTableRow row2 = new ViewTableRow("row2");

        ViewTableCell cell3 = new ViewTableCell("rowspan 2");
        cell3.setRowSpan(2);

        row2.addCell(cell3);
        row2.addCell(new ViewTableCell("cell 4"));
        row2.addCell(new ViewTableCell("cell 5"));
        tableComponent.addRow(row2);

        ViewTableRow row3 = new ViewTableRow("row3");
        row3.addCell(new ViewTableCell("cell6"));
        row3.addCell(new ViewTableCell("cell7"));
        row3.addCell(new ViewTableCell("cell8"));

        tableComponent.addRow(row3);

        tableComponent.setName("main table");

        createPDFAndCompareWithExpected(expectedOutputFilePath,
                                        outFilePath,
                                        tableComponent);

    }

}
