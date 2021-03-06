package org.lawrencebower.docgen.doc_examples.commercial_invoice;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class CommercialInvoiceRunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\doc-examples-context.xml");

        CommercialInvoice doc = (CommercialInvoice) context.getBean("commercialInvoice");

        DocumentView documentView = doc.getDocumentView();

        Document document = documentView.getDocument();

        PDFDocument pdfDocument = document.generatePDF();

        File outFile = new File("C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\CommercialInvoice_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
