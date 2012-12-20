package org.lawrencebower.docgen.doc_examples.delivery_note;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class DeliveryNoteRunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\doc-examples-context.xml");

        DeliveryNote doc = (DeliveryNote) context.getBean("deliveryNote");

        Document document = doc.getDocumentView().getDocument();

        PDFDocument pdfDocument = document.generatePDF();

        File outFile = new File("C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\DeliveryNote_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
