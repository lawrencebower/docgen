package org.lawrencebower.docgen.doc_examples.commercial_invoice.components;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.TableTextComponent;
import org.lawrencebower.docgen.core.document.component.position.HorizontalAlignment;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.document.component.table.layout_table.*;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;

import java.awt.*;

import static org.lawrencebower.docgen.web_logic.business.mapping.auto_mapped.AutoMappedField.*;

public class InvoiceTableBuilder {

    private final ComponentBuilder componentBuilder;
    private final DocumentViewBuilder documentViewBuilder;

    public InvoiceTableBuilder(ComponentBuilder componentBuilder,
                               DocumentViewBuilder documentViewBuilder) {

        this.componentBuilder = componentBuilder;
        this.documentViewBuilder = documentViewBuilder;
    }

    public TableComponent buildInvoiceTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("invoice table");
        tableBuilder.setTablePadding(0);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);

        TableTextComponent headerComponent = new TableTextComponent(HorizontalAlignment.CENTER,
                                                                    "Commercial Invoice");
        LayoutHeaderCell headerCell = new LayoutHeaderCell(headerComponent);
        headerCell.setPadding(3);
        headerCell.setBackgroundColor(Color.LIGHT_GRAY);
        headerCell.setColSpan(2);
        headerRow.addCell(headerCell);

        tableBuilder.setHeaderRow(headerRow);

        TableComponent shippedFrom1Table = makeShippedFrom1Table();
        TableComponent shippedFrom2Table = makeShippedFrom2Table();
        TableComponent shippedToTable = makeShippedToTable();
        TableComponent soldToTable = makeSoldToTable();

        tableBuilder.addRowWithComponents(shippedFrom1Table, shippedFrom2Table);

        LayoutRow row2 = new LayoutRow();
        row2.addCell(new LayoutCell(shippedToTable));
        row2.addCell(new LayoutCell(soldToTable));

        tableBuilder.addRowWithComponents(shippedToTable, soldToTable);

        tableBuilder.setWidthPercentage(100);

        tableBuilder.setRenderBorder(true);

        return tableBuilder.getTable();

    }

    private TableComponent makeSoldToTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("sold to");

        tableBuilder.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedToCell = new LayoutHeaderCell("SOLD TO");
        shippedToCell.setBackgroundColor(Color.LIGHT_GRAY);
        shippedToCell.setColSpan(2);
        headerRow.addCell(shippedToCell);
        tableBuilder.setHeaderRow(headerRow);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent(BUSINESS_NAME.getName(), "blah");
        tableBuilder.createRowWithLabelAndValue("Name:", textComponent);
        documentViewBuilder.addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_CONTACT_NAME.getName(), "David Davidson");
        tableBuilder.createRowWithLabelAndValue("Contact Name:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_PHONE.getName(), "123456788");
        tableBuilder.createRowWithLabelAndValue("Phone:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_ADDRESS.getName(),
                                                 "Suites 11 & 12\n" +
                                                 "Church Farm,\n" +
                                                 "Maris Lane,\n" +
                                                 "Trumpington, CB29LG");
        tableBuilder.createRowWithLabelAndValue("Company address:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(BUSINESS_COUNTRY.getName(), "UNITED KINGDOM");
        tableBuilder.createRowWithLabelAndValue("Country:", textComponent);
        addViewableComponent(textComponent);

        return tableBuilder.getTable();
    }

    private TableComponent makeShippedToTable() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("shipped to");

        tableBuilder.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedToCell = new LayoutHeaderCell("SHIPPED TO");
        shippedToCell.setBackgroundColor(Color.LIGHT_GRAY);
        shippedToCell.setColSpan(2);
        headerRow.addCell(shippedToCell);
        tableBuilder.setHeaderRow(headerRow);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent(CUSTOMER_NAME.getName(), "Frank");
        tableBuilder.createRowWithLabelAndValue("Name:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_CONTACT_NAME.getName(), "Billy Bob Bobson");
        tableBuilder.createRowWithLabelAndValue("Contact Name:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_PHONE.getName(), "123456788");
        tableBuilder.createRowWithLabelAndValue("Phone:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_ADDRESS.getName(),
                                                 "Suites 11 & 12\n" +
                                                 "Church Farm,\n" +
                                                 "Maris Lane,\n" +
                                                 "Trumpington, CB29LG");
        tableBuilder.createRowWithLabelAndValue("Company address:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(CUSTOMER_COUNTRY.getName(), "UNITED KINGDOM");
        tableBuilder.createRowWithLabelAndValue("Country:", textComponent);
        addViewableComponent(textComponent);

        return tableBuilder.getTable();
    }

    private TableComponent makeShippedFrom2Table() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("shipped from 2");

        tableBuilder.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        LayoutHeaderCell shippedFromCell = new LayoutHeaderCell();
        shippedFromCell.setColSpan(2);
        headerRow.addCell(shippedFromCell);
        tableBuilder.setHeaderRow(headerRow);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent("date", "29th May 2012");
        tableBuilder.createRowWithLabelAndValue("Date:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("refNumber", "154487945");
        tableBuilder.createRowWithLabelAndValue("Reference/Order No:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Airbill Number:", "45678945");
        tableBuilder.createRowWithLabelAndValue("Airbill Number:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent("Reason For Export:", "SOLD");
        tableBuilder.createRowWithLabelAndValue("Reason For Export:", textComponent);

        textComponent = createTableTextComponent("Incoterms:", "FOB");
        tableBuilder.createRowWithLabelAndValue("Incoterms:", textComponent);

        return tableBuilder.getTable();
    }

    private TableComponent makeShippedFrom1Table() {

        LayoutTableBuilder tableBuilder = new LayoutTableBuilder("shipped from");

        tableBuilder.setWidthPercentage(100);

        LayoutHeaderRow headerRow = new LayoutHeaderRow();
        headerRow.setRenderHeader(true);
        LayoutHeaderCell shippedFromCell = new LayoutHeaderCell("SHIPPED FROM");
        shippedFromCell.setBackgroundColor(Color.LIGHT_GRAY);
        shippedFromCell.setColSpan(2);
        headerRow.addCell(shippedFromCell);
        tableBuilder.setHeaderRow(headerRow);

        TableTextComponent textComponent;

        textComponent = createTableTextComponent(VENDOR_NAME.getName(), "Acme Ltd");
        tableBuilder.createRowWithLabelAndValue("Name:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_TAX_ID.getName(), "154487945");
        tableBuilder.createRowWithLabelAndValue("TAX/VAT NUMBER:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_CONTACT_NAME.getName(), "David Davidson");
        tableBuilder.createRowWithLabelAndValue("Contact Name:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_PHONE.getName(), "+44 (0)1223 655577");
        tableBuilder.createRowWithLabelAndValue("Phone:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_ADDRESS.getName(),
                                                 "Building 3\n" +
                                                 "Cathedral Farm,\n" +
                                                 "Skipton Lane,\n" +
                                                 "Ecles, CB33LG");
        tableBuilder.createRowWithLabelAndValue("Company address:", textComponent);
        addViewableComponent(textComponent);

        textComponent = createTableTextComponent(VENDOR_COUNTRY.getName(), "UNITED KINGDOM");
        tableBuilder.createRowWithLabelAndValue("Country:", textComponent);
        addViewableComponent(textComponent);


        return tableBuilder.getTable();
    }

    private void addViewableComponent(DocComponent component) {
        documentViewBuilder.addViewableComponent(component);
    }

    private TableTextComponent createTableTextComponent(String name, String value) {
        return componentBuilder.createTableTextComponent(name, value);
    }
}
