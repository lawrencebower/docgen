package org.lawrencebower.docgen.core.document.component.table;

public interface TableHeaderRow<T extends HeaderCell> extends TableRow<T> {

    int[] getColumnWidths();

    void setColumnWidths(Integer... i);

    boolean isRenderHeader();

    void setRenderHeader(boolean renderHeader);
}
