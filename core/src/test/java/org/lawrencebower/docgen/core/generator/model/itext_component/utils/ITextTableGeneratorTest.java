package org.lawrencebower.docgen.core.generator.model.itext_component.utils;

import com.lowagie.text.pdf.PdfPTable;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableComponent;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static junit.framework.Assert.assertEquals;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/core-test-context.xml")
public class ITextTableGeneratorTest {

    @Autowired
    private ITextTableGenerator tableGenerator;

    @Test
    public void testMakeTable_standardComponent_returnsValidRowCount() {

        LayoutTableComponent tableComponent = LayoutTableGenerator.makeLayoutTableComponent(3, 3);
        PdfPTable iTextTable = tableGenerator.generateTable(tableComponent);
        assertEquals(4, iTextTable.getRows().size());//includes header
    }

    @Test
    public void testMakeTable_standardComponent_returnsValidColCount() {

        LayoutTableComponent tableComponent = LayoutTableGenerator.makeLayoutTableComponent(3, 3);
        PdfPTable iTextTable = tableGenerator.generateTable(tableComponent);
        assertEquals(3, iTextTable.getNumberOfColumns());
    }

    @Test
    public void testMakeTable_setWidthPercentage_successfullySet() {

        LayoutTableComponent tableComponent = LayoutTableGenerator.makeLayoutTableComponent(3, 3);
        tableComponent.setWidthPercentage(12.5f);
        PdfPTable iTextTable = tableGenerator.generateTable(tableComponent);
        assertEquals(12.5f, iTextTable.getWidthPercentage());
    }

    @Test
    public void testMakeTable_validColumnWidths_noError() {
        LayoutTableComponent tableComponent = LayoutTableGenerator.makeLayoutTableComponent(3, 3);
        tableComponent.getHeaderRow().setColumnWidths(20,20,60);
        tableGenerator.generateTable(tableComponent);
    }

    @Test
    public void testMakeTable_badColumnWidths_throwsError() {
        try {
            LayoutTableComponent tableComponent = LayoutTableGenerator.makeLayoutTableComponent(3, 3);
            tableComponent.getHeaderRow().setColumnWidths(20,20,60,10);//different number of specified widths to columns
            tableGenerator.generateTable(tableComponent);
        } catch (DocGenException e) {
            assertEquals("Specified number of column widths '4' does not match the number of columns - '3'", e.getMessage());
        }
    }

}
