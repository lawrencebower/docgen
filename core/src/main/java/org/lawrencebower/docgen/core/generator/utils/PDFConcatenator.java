package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.BadPdfFormatException;
import com.lowagie.text.pdf.PdfCopy;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.*;
import java.util.List;

public class PDFConcatenator {

    private Document document;

    public void concatenatePDFs(List<File> pdfs, File output) {

        checkFiles(pdfs);

        document = new Document();

        PdfCopy copy = makePDFCopy(output);

        document.open();

        for (File pdf : pdfs) {

            PdfReader reader = createReader(pdf);

            addPagesInPDF(copy, reader);

            freeReader(copy, reader);
        }

        document.close();

    }

    private PdfCopy makePDFCopy(File output) {

        try {
            FileOutputStream outputStream = new FileOutputStream(output);

            return new PdfCopy(document, outputStream);
        } catch (FileNotFoundException | DocumentException e) {
            throw new DocGenException(e);
        }
    }

    private void addPagesInPDF(PdfCopy copy,
                               PdfReader reader) {

        int totalPageNumber = reader.getNumberOfPages();

        for (int page = 0; page < totalPageNumber; ) {

            PdfImportedPage pageFromSource = copy.getImportedPage(reader, ++page);

            addPage(copy, pageFromSource);
        }
    }

    private void addPage(PdfCopy copy, PdfImportedPage pageFromSource) {
        try {
            copy.addPage(pageFromSource);
        } catch (IOException | BadPdfFormatException e) {
            throw new DocGenException(e);
        }
    }

    private void freeReader(PdfCopy copy, PdfReader reader) {
        try {
            copy.freeReader(reader);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    private PdfReader createReader(File pdf) {
        try {
            FileInputStream instream = new FileInputStream(pdf);
            return new PdfReader(instream);
        } catch (IOException e) {
            throw new DocGenException(e);
        }
    }

    private void checkFiles(List<File> pdfs) {

        if(pdfs.isEmpty()){
            throw new DocGenException("No source files specified");
        }

        for (File pdf : pdfs) {
            checkFilExists(pdf);
        }
    }

    private void checkFilExists(File file) {
        if(!file.exists()){
            String message = String.format("file does not exist '%s'", file.getPath());
            throw new DocGenException(message);
        }
    }

}
