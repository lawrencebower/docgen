package org.lawrencebower.docgen.doc_examples.commercial_invoice;

import org.lawrencebower.docgen.core.document.ComponentBuilder;
import org.lawrencebower.docgen.core.document.component.DocComponent;
import org.lawrencebower.docgen.core.document.component.LineComponent;
import org.lawrencebower.docgen.core.document.component.NewLineComponent;
import org.lawrencebower.docgen.core.document.component.TextComponent;
import org.lawrencebower.docgen.core.document.component.table.TableComponent;
import org.lawrencebower.docgen.core.generator.custom.CustomDocument;
import org.lawrencebower.docgen.core.generator.custom.CustomDocumentBuilder;
import org.lawrencebower.docgen.doc_examples.commercial_invoice.components.InvoiceTableBuilder;
import org.lawrencebower.docgen.doc_examples.commercial_invoice.components.PackageInformationTableBuilder;
import org.lawrencebower.docgen.doc_examples.commercial_invoice.components.SignatureTableBuilder;
import org.lawrencebower.docgen.doc_examples.commercial_invoice.components.TotalsTableBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewBuilder;
import org.lawrencebower.docgen.web_model.view.document.DocumentViewImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.io.Resource;

public class CommercialInvoice {

    @Autowired
    private ComponentBuilder componentBuilder;
    @Autowired
    private CustomDocumentBuilder documentBuilder;
    @Autowired
    DocumentViewBuilder documentViewBuilder;
    @javax.annotation.Resource
    @Qualifier("signatureResource")
    private Resource signatureResource;

    public static final String INVOICE_NAME = "invoice";
    public static final String TOTAL_VALUE_NAME = "totalValue";

    private void prepareComponents() {

        initDocumentBuilders();

        TableComponent invoiceTable = makeInvoiceTable();
        addComponent(invoiceTable);

        addNewLine();

        TableComponent packageInformationTable = makePackageInformationTable();

        addComponent(packageInformationTable);

        TableComponent totalsTable = makeTotalsTable();

        addComponent(totalsTable);

        addNewLine();

        TableComponent signatureTable = makeSignatureTable();

        addComponent(signatureTable);

        addComponent(new LineComponent(70));

        DocComponent component;

        component = createTextComponent("Signature of exporter (print and sign)\n" +
                                        "I declare all the information to be accurate and correct");
        addComponent(component);

        addNewLine();

        component = createTextComponent("date", "29th May 2012");
        addComponent(component);
        addViewableComponent(component);

        addComponent(new LineComponent(30));

        component = createTextComponent("Date");
        addComponent(component);

    }

    private void initDocumentBuilders() {
        documentBuilder.createDocument(INVOICE_NAME);
        documentViewBuilder.createDocument();
    }

    private TableComponent makeInvoiceTable() {

        InvoiceTableBuilder invoiceTableBuilder = new InvoiceTableBuilder(componentBuilder,
                                                                          documentViewBuilder);
        return invoiceTableBuilder.buildInvoiceTable();
    }

    private TableComponent makePackageInformationTable() {

        PackageInformationTableBuilder packageInformationTableBuilder = new PackageInformationTableBuilder();

        return packageInformationTableBuilder.buildTablePackageInformationTable(documentViewBuilder);
    }

    private TableComponent makeTotalsTable() {

        TotalsTableBuilder totalsTableBuilder = new TotalsTableBuilder(documentViewBuilder,
                                                                       componentBuilder);

        return totalsTableBuilder.buildTotalsTable();
    }

    private TableComponent makeSignatureTable() {

        SignatureTableBuilder signatureTableBuilder = new SignatureTableBuilder();

        return signatureTableBuilder.buildTable(signatureResource);
    }

    private TextComponent createTextComponent(String value) {
        return componentBuilder.createTextComponentWithValue(value);
    }

    private TextComponent createTextComponent(String name, String value) {
        return componentBuilder.createTextComponent(name, value);
    }

    private void addComponent(DocComponent component) {
        documentBuilder.addComponent(component);
    }

    private void addViewableComponent(DocComponent component) {
        documentViewBuilder.addViewableComponent(component);
    }

    private void addNewLine() {
        NewLineComponent newLine = componentBuilder.createNewLine();
        addComponent(newLine);
    }


    public DocumentViewImpl getDocumentView() {
        DocumentViewImpl documentView = documentViewBuilder.getDocumentView();
        CustomDocument document = documentBuilder.getDocument();
        documentView.setDocument(document);
        return documentView;
    }
}
