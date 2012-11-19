package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class CommercialInvoiceRunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\usecase-context.xml");

        CommercialInvoice doc = (CommercialInvoice) context.getBean("commercialInvoice");

        DocumentInfo docInfo = doc.getDocInfoView().getDocumentInfo();

        PDFDocument pdfDocument = docInfo.generatePDF();

        File outFile = new File("C:\\GitHub\\docgen\\use_cases\\src\\main\\resources\\CommercialInvoice_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
