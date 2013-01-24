package org.lawrencebower.docgen.core.generator.model.itext_component.utils;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.view_table.*;

public class WebTableGenerator {

    public static ViewTableComponent makeWebTableComponent(int colNumber, int rowNumber) {

        ViewTableComponent tableComponent = new ViewTableComponent("Table Name");

        WebTableHeaderRow headerRow = new WebTableHeaderRow();
        fillHeaderRow(headerRow, "col", colNumber);
        tableComponent.setHeaderRow(headerRow);

        for (int i = 0; i < rowNumber; i++) {
            ViewTableRow tableRow = new ViewTableRow("row" + i);
            fillTableRow(tableRow, colNumber);
            tableComponent.addRow(tableRow);
        }

        tableComponent.setRenderBorder(true);
        tableComponent.setAlignment(HorizontalAlignment.CENTER);

        return tableComponent;
    }

    static void fillTableRow(ViewTableRow tableRow,
                             int colNumber) {
        fillTableRow(tableRow,
                     "",
                     colNumber);
    }

    static void fillTableRow(ViewTableRow tableRow,
                             String prefix,
                             int colNumber) {

        for (int i = 0; i < colNumber; i++) {
            ViewTableCell layoutCell = getLayoutCell(prefix, i);
            tableRow.addCell(layoutCell);
        }
    }

    static void fillHeaderRow(WebTableHeaderRow tableRow,
                              String prefix,
                              int colNumber) {

        for (int i = 0; i < colNumber; i++) {
            ViewHeaderCell layoutCell = getLayoutHeaderCell(prefix, i);
            tableRow.addCell(layoutCell);
        }
    }

    private static ViewTableCell getLayoutCell(String prefix, int number) {
        return new ViewTableCell(prefix + number);
    }

    private static ViewHeaderCell getLayoutHeaderCell(String prefix, int number) {
        return new ViewHeaderCell(prefix + number);
    }
}
