package org.lawrencebower.docgen.doc_examples;

import org.lawrencebower.docgen.core.document.DocumentInfo;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.springframework.context.support.ClassPathXmlApplicationContext;

import java.io.File;

public class FDARunner {

    public static void main(String[] args) {

        ClassPathXmlApplicationContext context = new ClassPathXmlApplicationContext("\\META-INF\\usecase-context.xml");

        FDA_2887 doc = (FDA_2887) context.getBean("fda2887");

        doc.setComponentValuesAndRenderBorder();

        DocumentInfo docInfo = doc.getDocInfoView().getDocumentInfo();

        PDFDocument pdfDocument = docInfo.generatePDF();

        File outFile = new File("C:\\GitHub\\use_cases\\src\\main\\resources\\FDA-2877_output.pdf");

        pdfDocument.writeToFile(outFile);
    }
}
