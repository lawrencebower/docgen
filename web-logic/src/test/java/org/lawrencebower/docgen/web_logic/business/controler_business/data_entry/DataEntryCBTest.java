package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.PDFDocumentImpl;
import org.lawrencebower.docgen.core.generator.utils.DocGenFileUtils;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_model.view.contact.BusinessSelection;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.contact.CustomerSelection;
import org.lawrencebower.docgen.web_model.view.document.*;
import org.lawrencebower.docgen.web_model.view.product.ProductSelection;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.lawrencebower.docgen.web_model.view.view_factory.ViewFactory;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.*;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = "classpath:META-INF/web-logic-test-context.xml")
public class DataEntryCBTest {

    @Autowired
    DataEntryCB dataEntryBusiness;
    @Autowired
    DocumentSetFactory documentSetFactory;
    @Autowired
    private DocGenFileUtils fileUtils;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    @Mock
    CustomerSelection mockCustomerSelection;
    @Mock
    BusinessSelection mockBusinessSelection;
    @Mock
    ArgumentCaptor<ArrayList<ProductView>> mockProducts;
    @Mock
    ProductSelection productSelection;

    @Before
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testGetDocumentsForViewing_validData_returnsCorrectDocs() throws Exception {

        ViewFactory mockFactory = mock(ViewFactory.class);

        List<ProductView> products = Arrays.asList(mock(ProductView.class),
                                                   mock(ProductView.class),
                                                   mock(ProductView.class));

        when(productSelection.getProducts()).thenReturn(products);

        DocumentView docView1 = mockDocumentView("doc1");
        DocumentView docView2 = mockDocumentView("doc2");
        DocumentView docView3 = mockDocumentView("doc3");

        List<DocumentView> list1 = Arrays.asList(docView1, docView2);
        List<DocumentView> list2 = Arrays.asList(docView2, docView3);

        given(mockFactory.getDocumentsForCustomerAndProduct(any(ContactView.class),
                                                            any(ProductView.class))).willReturn(list1, list1, list2);

        dataEntryBusiness.setViewFactory(mockFactory);

        DocumentSet docSet =
                dataEntryBusiness.getDocumentsForViewing(mockCustomerSelection, productSelection);

        List<DocumentView> forViewing = docSet.getDocumentsAsList();

        assertEquals(3, forViewing.size());
        assertEquals("doc1", forViewing.get(0).getName());
        assertEquals("doc2", forViewing.get(1).getName());
        assertEquals("doc3", forViewing.get(2).getName());
    }

    @Test
    public void testCreatePDFs_validData_pdfCreated() throws Exception {

        DocumentView docView1 = mockDocumentView("doc1");
        DocumentView docView2 = mockDocumentView("doc2");
        DocumentSet documentSet = documentSetFactory.createDocumentInfoSet(docView1, docView2);

        PDFDocument pdf1 = mock(PDFDocument.class);
        PDFDocument pdf2 = mock(PDFDocument.class);

        when(docView1.generatePDF()).thenReturn(pdf1);
        when(docView2.generatePDF()).thenReturn(pdf2);

        when(docView1.generatePDF()).thenReturn(pdf1);
        when(docView2.generatePDF()).thenReturn(pdf2);

        List<PDFDocument> results = dataEntryBusiness.createPDFs(documentSet);

        verify(pdf1).setName("doc1");
        verify(pdf2).setName("doc2");

        assertEquals(2, results.size());
    }

    @Test
    public void testWritePDFsToFiles_validData_correctFileNamesSet() throws Exception {

        PDFDocument pdf1 = new PDFDocumentImpl(new byte[]{});
        PDFDocument pdf2 = new PDFDocumentImpl(new byte[]{});

        String pdf1Name = "pdf1";
        String pdf2Name = "pdf2";

        pdf1.setName(pdf1Name);
        pdf2.setName(pdf2Name);

        List<PDFDocument> pdfDocuments = Arrays.asList(pdf1, pdf2);

        dataEntryBusiness.writePDFsToFiles(pdfDocuments);

        String file1 = pdfDocuments.get(0).getFile().getPath();
        String file2 = pdfDocuments.get(1).getFile().getPath();

        assertEquals(fileRoot + "pdf1.pdf", file1);
        assertEquals(fileRoot + "pdf2.pdf", file2);

        fileUtils.deleteQuietly(new File(file1));
        fileUtils.deleteQuietly(new File(file2));
    }

    @Test
    public void testWritePDFsToFiles_nameExtensionSet_correctFileNamesSet() throws Exception {

        PDFDocument pdf1 = new PDFDocumentImpl(new byte[]{});
        PDFDocument pdf2 = new PDFDocumentImpl(new byte[]{});

        String pdf1Name = "pdf1";
        String pdf2Name = "pdf2";
        String pdf2Extension = "extension";

        pdf1.setName(pdf1Name);
        pdf2.setName(pdf2Name);
        pdf2.setNameExtension(pdf2Extension);

        List<PDFDocument> pdfDocuments = Arrays.asList(pdf1, pdf2);

        dataEntryBusiness.writePDFsToFiles(pdfDocuments);

        String file1 = pdfDocuments.get(0).getFile().getPath();
        String file2 = pdfDocuments.get(1).getFile().getPath();

        assertEquals(fileRoot + "pdf1.pdf", file1);
        assertEquals(fileRoot + "pdf2_extension.pdf", file2);

        fileUtils.deleteQuietly(new File(file1));
        fileUtils.deleteQuietly(new File(file2));
    }

    @Test
    public void testMakeConcatenatedFile_validParams_returnsCorrectFileName() throws Exception {
        dataEntryBusiness.setPdfConcatenator(mock(PDFConcatenator.class));
        File file = dataEntryBusiness.makeConcatenatedFile(new ArrayList<PDFDocument>());
        assertEquals(fileRoot + ViewConstants.CONCATENATED_FILE_NAME, file.getPath());
    }

    @Test
    public void testMapAutoMapFields_emptyDocs_errorThrown() throws Exception {
        try {
            DocumentSet documentSet = documentSetFactory.createDocumentInfoSet();
            dataEntryBusiness.mapAutoMapComponents(documentSet,
                                                   mockCustomerSelection,
                                                   mockBusinessSelection);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(DocumentSetImpl.NO_DOCUMENTS_SELECTED, message);
        }

    }

    @Test
    public void testGetDocumentsForViewing_customerCheckCalled() throws Exception {
        dataEntryBusiness.getDocumentsForViewing(mockCustomerSelection, productSelection);
        verify(productSelection, times(1)).checkProductsSet();
    }

    @Test
    public void testGetDocumentsForViewing_productCheckCalled() throws Exception {
        dataEntryBusiness.getDocumentsForViewing(mockCustomerSelection, productSelection);
        verify(productSelection, times(1)).checkProductsSet();
    }

    @Test
    public void testMapAutoMapFields_customerSelectionCheckCalled() throws Exception {
        dataEntryBusiness.mapAutoMapComponents(mock(DocumentSetImpl.class),
                                               mockCustomerSelection,
                                               mockBusinessSelection);

        verify(mockCustomerSelection, times(1)).checkCustomerSet();
    }

    @Test
    public void testMapAutoMapFields_businessSelectionCheckCalled() throws Exception {
        dataEntryBusiness.mapAutoMapComponents(mock(DocumentSetImpl.class),
                                               mockCustomerSelection,
                                               mockBusinessSelection);

        verify(mockBusinessSelection, times(1)).checkBusinessSet();
    }

    //MOCKING

    private DocumentView mockDocumentView(String docName) {
        DocumentView docView = mock(DocumentViewImpl.class);
        when(docView.getName()).thenReturn(docName);
        return docView;
    }

}
