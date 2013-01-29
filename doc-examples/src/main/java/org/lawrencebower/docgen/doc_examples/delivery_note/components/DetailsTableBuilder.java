package org.lawrencebower.docgen.doc_examples.delivery_note.components;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutHeaderCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;

import static org.lawrencebower.docgen.doc_examples.delivery_note.DeliveryNote.ACME_BLUE;

public class DetailsTableBuilder {

    private final ComponentBuilder componentBuilder;
    private final DocumentViewBuilder documentViewBuilder;

    public DetailsTableBuilder(ComponentBuilder componentBuilder,
                               DocumentViewBuilder documentViewBuilder) {

        this.componentBuilder = componentBuilder;
        this.documentViewBuilder = documentViewBuilder;
    }

    public TableComponent buildDetailsTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("Details table");
        tableBuilder.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);

        LayoutHeaderCell headerCell = new LayoutHeaderCell("DETAILS AND OBSERVATIONS");
        headerCell.setBackgroundColor(ACME_BLUE);
        headerRow.addCell(headerCell);

        tableBuilder.setHeaderRow(headerRow);

        TableTextComponent detailsComponent =
                componentBuilder.createTableTextComponent("observations", "THIS COMPLETES THE ORDER");

        tableBuilder.addRowWithComponents(detailsComponent);

        documentViewBuilder.addViewableComponent(detailsComponent);

        tableBuilder.setRenderBorder(true);

        return tableBuilder.getTable();
    }
}
