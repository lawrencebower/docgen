package org.lawrencebower.docgen.core.generator.model.itext_component.utils;

import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.layout_table.*;

public class LayoutTableGenerator {

    public static LayoutTableComponent makeLayoutTableComponent(int colNumber, int rowNumber) {

        LayoutTableComponent tableComponent = new LayoutTableComponent("Table Name");

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        fillHeaderRow(headerRow, "col", colNumber);
        tableComponent.setHeaderRow(headerRow);

        for (int i = 0; i < rowNumber; i++) {
            LayoutRow tableRow = new LayoutRow();
            fillTableRow(tableRow, colNumber);
            tableComponent.addRow(tableRow);
        }

        tableComponent.setRenderBorder(true);
        tableComponent.setAlignment(HorizontalAlignment.CENTER);

        return tableComponent;
    }

    static void fillTableRow(LayoutRow tableRow,
                             int colNumber) {
        fillTableRow(tableRow,
                     "",
                     colNumber);
    }

    static void fillTableRow(LayoutRow tableRow,
                             String prefix,
                             int colNumber) {

        for (int i = 0; i < colNumber; i++) {
            LayoutCell layoutCell = getLayoutCell(prefix, i);
            tableRow.addCell(layoutCell);
        }
    }

    static void fillHeaderRow(LayoutHeaderRow tableRow,
                              String prefix,
                              int colNumber) {

        for (int i = 0; i < colNumber; i++) {
            LayoutHeaderCell layoutCell = getLayoutHeaderCell(prefix, i);
            tableRow.addCell(layoutCell);
        }
    }

    private static LayoutCell getLayoutCell(String prefix, int number) {
        return new LayoutCell(prefix + number);
    }

    private static LayoutHeaderCell getLayoutHeaderCell(String prefix, int number) {
        return new LayoutHeaderCell(prefix + number);
    }
}
