package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class DeliveryNoteRunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\usecase-context.xml");

        DeliveryNote doc = (DeliveryNote) context.getBean("deliveryNote");

        Document document = doc.getDocument().getDocument();

        PDFDocument pdfDocument = document.generatePDF();

        File outFile = new File("C:\\GitHub\\docgen\\use_cases\\src\\main\\resources\\DeliveryNote_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
