package org.lawrencebower.docgen.doc_examples.fcc_740;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class FCC740Runner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\doc-examples-context.xml");

        FCC_740 doc = (FCC_740) context.getBean("fcc740");

//        doc.setComponentValuesAndRenderBorder();

        DocumentView documentView = doc.getDocumentView();

        Document document = documentView.getDocument();

        PDFDocument pdfDocument = document.generatePDF();

        File outFile = new File("C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\FCC_740_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
