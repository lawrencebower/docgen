package org.lawrencebower.docgen.core.generator.utils;

import com.lowagie.text.Document;
import com.lowagie.text.DocumentException;
import com.lowagie.text.pdf.BadPdfFormatException;
import com.lowagie.text.pdf.PdfCopy;
import com.lowagie.text.pdf.PdfImportedPage;
import com.lowagie.text.pdf.PdfReader;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;

import java.io.*;
import java.util.List;

public class PDFConcatenator {

    public void concatenatePDFs(List<PDFDocument> pdfs, File output) {

        checkFiles(pdfs);

        Document document = new Document();

        PdfCopy copy = makePDFCopy(output, document);

        document.open();

        for (PDFDocument pdf : pdfs) {
            addDocumentToCopy(copy, pdf);
        }

        document.close();

    }

    private void addDocumentToCopy(PdfCopy copy, PDFDocument pdf) {

        File file = pdf.getFile();
        int copyNumber = pdf.getCopyNumber();

        int count = 0;

        while (count < copyNumber) {
            PdfReader reader = createReader(file);
            addPagesInPDF(copy, reader);
            freeReader(copy, reader);
            count++;
        }
    }

    private PdfCopy makePDFCopy(File output, Document document) {

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

    private void checkFiles(List<PDFDocument> pdfs) {

        if (pdfs.isEmpty()) {
            throw new DocGenException("No source files specified");
        }

        for (PDFDocument pdf : pdfs) {
            File file = pdf.getFile();
            checkFilExists(file);
        }
    }

    private void checkFilExists(File file) {
        if (!file.exists()) {
            String message = String.format("file does not exist '%s'", file.getPath());
            throw new DocGenException(message);
        }
    }

}
