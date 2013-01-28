package org.lawrencebower.docgen.core.document.component.table.layout_table;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.AbstractTableBuilder;

public class LayoutTableBuilder extends AbstractTableBuilder<LayoutTableComponent> {

    public LayoutTableBuilder() {
        this.table = new LayoutTableComponent();
    }

    public LayoutTableBuilder(String tableName) {
        this.table = new LayoutTableComponent(tableName);
    }

    public LayoutTableComponent getTable() {
        return table;
    }

    public void setHeaderRow(LayoutHeaderRow headerRow) {
        table.setHeaderRow(headerRow);
    }

    public void addRowWithComponents(DocComponent... components) {
        LayoutRow row = new LayoutRow();
        for (DocComponent component : components) {
            LayoutCell cell = makeCell(component);
            row.addCell(cell);
        }
       table.addRow(row);
    }

    public LayoutCell makeCell(DocComponent component) {
        return new LayoutCell(component);
    }

    public void createRowWithLabelAndValue(String label, DocComponent component) {
        TableTextComponent textComponent = new TableTextComponent(label);
        addRowWithComponents(textComponent, component);
    }

    public LayoutRow makeRow() {
        return new LayoutRow();
    }

    public void makeEmptyHeaderRowWithColSpans(int... colspans) {

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        LayoutHeaderCell cell = new LayoutHeaderCell();

        for (int colspan : colspans) {
            cell.setColSpan(colspan);
            headerRow.addCell(cell);
        }

        setHeaderRow(headerRow);
    }

    public void makeEmptyHeaderRowWithColWidths(int... colWidths) {

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        LayoutHeaderCell cell = new LayoutHeaderCell();

        for (int colWidth : colWidths) {
            cell.setColumnWidth(colWidth);
            headerRow.addCell(cell);
        }

        setHeaderRow(headerRow);
    }

    public void addRow(LayoutRow row) {
        table.addRow(row);
    }
}
