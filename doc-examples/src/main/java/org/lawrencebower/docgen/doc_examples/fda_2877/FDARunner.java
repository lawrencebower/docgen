package org.lawrencebower.docgen.doc_examples.fda_2877;

import org.lawrencebower.docgen.core.document.Document;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.web_model.view.document.DocumentView;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class FDARunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\doc-examples-context.xml");

        FDA_2877 doc = (FDA_2877) context.getBean("fda2877");

//        doc.setComponentValuesAndRenderBorder();

        DocumentView documentView = doc.getDocumentView();

        Document document = documentView.getDocument();

        PDFDocument pdfDocument = document.generatePDF();

        File outFile = new File("C:\\GitHub\\docgen\\doc-examples\\src\\main\\resources\\FDA-2877_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
