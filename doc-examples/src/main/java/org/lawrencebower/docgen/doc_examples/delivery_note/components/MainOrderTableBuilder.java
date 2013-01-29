package org.lawrencebower.docgen.doc_examples.delivery_note.components;

import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewHeaderCell;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewHeaderRow;
import org.lawrencebower.docgen.core.document.component.table.view_table.ViewTableBuilder;
import org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField;

import static org.lawrencebower.docgen.doc_examples.delivery_note.DeliveryNote.ACME_BLUE;
import static org.lawrencebower.docgen.web_logic.business.injection.product.ProductInjectionField.PRODUCT_QUANTITY;

public class MainOrderTableBuilder {

    public TableComponent buildMainOrderTable() {

        ViewTableBuilder tableBuilder = new ViewTableBuilder("Items table");
        tableBuilder.setWidthPercentage(100);

        ViewHeaderRow headerRow = new ViewHeaderRow();

        TextComponent qty = new TextComponent(HorizontalAlignment.CENTER, "QTY");
        String productQualityName = PRODUCT_QUANTITY.getName();
        ViewHeaderCell qtyCell = new ViewHeaderCell(productQualityName);
        qtyCell.setText("QTY");
        qtyCell.setColumnWidth(20);
        qtyCell.setBackgroundColor(ACME_BLUE);
        qtyCell.setComponent(qty);
        headerRow.addCell(qtyCell);

        ViewHeaderCell descriptionCell = new ViewHeaderCell(ProductInjectionField.PRODUCT_NAME_AND_DESCRIPTION.getName());
        descriptionCell.setText("DESCRIPTION");
        descriptionCell.setColumnWidth(80);
        descriptionCell.setBackgroundColor(ACME_BLUE);
        TextComponent desc = new TextComponent(HorizontalAlignment.CENTER, "DESCRIPTION");
        descriptionCell.setComponent(desc);
        headerRow.addCell(descriptionCell);

        tableBuilder.setHeaderRow(headerRow);

        tableBuilder.setRenderBorder(true);

        return tableBuilder.getTable();
    }
}
