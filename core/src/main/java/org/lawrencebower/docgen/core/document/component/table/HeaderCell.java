package org.lawrencebower.docgen.core.document.component.table;

public interface HeaderCell extends TableCell {

    Integer getColumnWidth();

    void setColumnWidth(Integer width);

    boolean hasColumnWidth();
}
