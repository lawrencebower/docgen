package org.lawrencebower.docgen.doc_examples.delivery_note.components;

import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutCell;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutRow;
import org.lawrencebower.docgen.core.document.component.table.layout_table.LayoutTableBuilder;
import org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;

public class ToTableBuilder {

    private DocumentViewBuilder documentViewBuilder;

    public ToTableBuilder(DocumentViewBuilder documentViewBuilder) {
        this.documentViewBuilder = documentViewBuilder;
    }

    public DocComponent buildToTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("to table");

        tableBuilder.makeEmptyHeaderRowWithColWidths(1, 9);

        LayoutRow row = tableBuilder.makeRow();

        LayoutCell toCell = new LayoutCell("To");
        toCell.setRowSpan(4);
        row.addCell(toCell);

        TableTextComponent nameComponent = new TableTextComponent("Lawrence Bower");
        nameComponent.setName(AutoMappedField.CUSTOMER_CONTACT_NAME.getName());
        LayoutCell nameCell = new LayoutCell(nameComponent);
        documentViewBuilder.addViewableComponent(nameComponent);
        row.addCell(nameCell);

        tableBuilder.addRow(row);

        TableTextComponent companyComponent = new TableTextComponent("Acme ltd");
        companyComponent.setName(AutoMappedField.CUSTOMER_NAME.getName());
        documentViewBuilder.addViewableComponent(companyComponent);
        tableBuilder.addRowWithComponents(companyComponent);

        TableTextComponent addressComponent = new TableTextComponent("36 BillyBob Street\nEssex");
        addressComponent.setName(AutoMappedField.CUSTOMER_ADDRESS.getName());
        documentViewBuilder.addViewableComponent(addressComponent);
        tableBuilder.addRowWithComponents(addressComponent);

        TableTextComponent countryComponent = new TableTextComponent("UK");
        countryComponent.setName(AutoMappedField.CUSTOMER_COUNTRY.getName());
        documentViewBuilder.addViewableComponent(countryComponent);
        tableBuilder.addRowWithComponents(companyComponent);

        tableBuilder.setRenderBorder(false);

        return tableBuilder.getTable();
    }
}
