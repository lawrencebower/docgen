package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class DeliveryNoteRunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\usecase-context.xml");

        DeliveryNote doc = (DeliveryNote) context.getBean("deliveryNote");

        DocumentInfo docInfo = doc.getDocInfo().getDocumentInfo();

        PDFDocument pdfDocument = docInfo.generatePDF();

        File outFile = new File("C:\\GitHub\\docgen\\use_cases\\src\\main\\resources\\DeliveryNote_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
