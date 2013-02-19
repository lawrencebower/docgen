package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.doc_examples.delivery_note.DeliveryNote;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class DeliveryNoteRunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\doc-examples-test-context.xml");

        DeliveryNote doc = (DeliveryNote) context.getBean("deliveryNote");

        Document document = doc.getDocumentView().getDocument();

        PDFDocument pdfDocument = document.generatePDF();

        String fileRoot = (String) context.getBean("pdfOutputRoot");

        File outFile = new File(fileRoot + File.separator + "DeliveryNote_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
