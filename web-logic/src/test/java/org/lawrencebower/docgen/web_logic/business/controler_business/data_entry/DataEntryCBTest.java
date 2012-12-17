package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.model.PDFDocumentImpl;
import org.lawrencebower.docgen.core.generator.utils.DocGenFileUtils;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_logic.business.mapping.customer_product_document.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_logic.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_logic.view.contact.ContactView;
import org.lawrencebower.docgen.web_logic.view.document.DocumentSet;
import org.lawrencebower.docgen.web_logic.view.document.DocumentSetFactory;
import org.lawrencebower.docgen.web_logic.view.document.DocumentView;
import org.lawrencebower.docgen.web_logic.view.product.ProductView;
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
    ContactView mockCustomer;
    @Mock
    ContactView mockBusiness;
    @Mock
    ArgumentCaptor<ArrayList<ProductView>> mockProducts;

    @Before
    public void setup() {
        MockitoAnnotations.initMocks(this);
    }

    @Test
    public void testGetDocumentsForViewing_noCustomer_throwsError() throws Exception {
        try {
            ContactView selectedCustomer = null;
            dataEntryBusiness.getDocumentsForViewing(selectedCustomer, mockProducts.capture());
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_CUSTOMER_SELECTED, message);
        }
    }

    @Test
    public void testGetDocumentsForViewing_nullProducts_throwsError() throws Exception {
        try {
            List<ProductView> selectedProducts = null;
            dataEntryBusiness.getDocumentsForViewing(mockCustomer, selectedProducts);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_PRODUCTS_SELECTED, message);
        }
    }

    @Test
    public void testGetDocumentsForViewing_emptyProducts_throwsError() throws Exception {
        try {
            List<ProductView> selectedProducts = new ArrayList<>();
            dataEntryBusiness.getDocumentsForViewing(mockCustomer, selectedProducts);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_PRODUCTS_SELECTED, message);
        }
    }

    @Test
    public void testGetDocumentsForViewing_validData_returnsCorrectDocs() throws Exception {

        CustomerProduct_Document_Mappings mockMapper = mock(CustomerProduct_Document_Mappings.class);

        List<ProductView> products = Arrays.asList(mock(ProductView.class),
                                                   mock(ProductView.class),
                                                   mock(ProductView.class));

        DocumentView docView1 = mockDocumentView("doc1");
        DocumentView docView2 = mockDocumentView("doc2");
        DocumentView docView3 = mockDocumentView("doc3");

        List<DocumentView> list1 = Arrays.asList(docView1, docView2);
        List<DocumentView> list2 = Arrays.asList(docView2, docView3);

        given(mockMapper.getDocumentsForCustomerAndProduct(any(ContactView.class),
                                                           any(ProductView.class))).willReturn(list1, list1, list2);

        dataEntryBusiness.setCustomerProductMappings(mockMapper);

        DocumentSet docSet =
                dataEntryBusiness.getDocumentsForViewing(mockCustomer, products);

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
                                                   mockCustomer,
                                                   mockBusiness);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_DOCUMENTS_SELECTED, message);
        }

    }

    @Test
    public void testMapAutoMapFields_nullCustomer_errorThrown() throws Exception {
        try {
            dataEntryBusiness.mapAutoMapComponents(mock(DocumentSet.class),
                                                   null,
                                                   mockBusiness);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_CUSTOMER_SELECTED, message);
        }
    }

    @Test
    public void testMapAutoMapFields_nullBusiness_errorThrown() throws Exception {
        try {
            dataEntryBusiness.mapAutoMapComponents(mock(DocumentSet.class),
                                                   mockCustomer,
                                                   null);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_BUSINESS_SELECTED, message);
        }
    }

    //MOCKING

    private DocumentView mockDocumentView(String docName) {
        DocumentView docView = mock(DocumentView.class);
        when(docView.getName()).thenReturn(docName);
        return docView;
    }

}
