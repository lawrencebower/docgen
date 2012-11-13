package org.lawrencebower.docgen.web_logic.business.controler_business.data_entry;

import org.junit.Before;
import org.junit.runner.RunWith;
import org.lawrencebower.docgen.core.document.PDFDocument;
import org.lawrencebower.docgen.core.exception.DocGenException;
import org.lawrencebower.docgen.core.generator.utils.PDFConcatenator;
import org.lawrencebower.docgen.web_logic.business.mapping.CustomerProduct_Document_Mappings;
import org.lawrencebower.docgen.web_logic.business.utils.ViewUtils;
import org.lawrencebower.docgen.web_model.view.constants.ViewConstants;
import org.lawrencebower.docgen.web_model.view.contact.Contact;
import org.lawrencebower.docgen.web_model.view.contact.ContactView;
import org.lawrencebower.docgen.web_model.view.document_info.DocumentInfoView;
import org.lawrencebower.docgen.web_model.view.product.Product;
import org.lawrencebower.docgen.web_model.view.product.ProductView;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import org.junit.Test;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:META-INF/web-logic-test-context.xml"})
public class DataEntryCBTest {

    @Autowired
    DataEntryCB dataEntryBusiness;

    @Autowired
    @Qualifier("pdfOutputRoot")
    String fileRoot;

    @Mock
    ContactView mockCustomer;
    @Mock
    ContactView mockBusiness;
    @Mock
    ArgumentCaptor<ArrayList<ProductView>> mockProducts;
    @Mock
    ProductView mockProduct;

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

        DocumentInfoView docView1 = mockDocInfoView("doc1");
        DocumentInfoView docView2 = mockDocInfoView("doc2");
        DocumentInfoView docView3 = mockDocInfoView("doc3");

        List<DocumentInfoView> list1 = Arrays.asList(docView1, docView2);
        List<DocumentInfoView> list2 = Arrays.asList(docView2, docView3);

        given(mockMapper.getDocInfosForCustomerAndProduct(any(Contact.class),
                                                          any(Product.class))).willReturn(list1, list1, list2);

        dataEntryBusiness.setCustomerProductMappings(mockMapper);

        List<DocumentInfoView> forViewing =
                dataEntryBusiness.getDocumentsForViewing(mockCustomer, products);

        assertEquals(3, forViewing.size());
        assertEquals("doc1", forViewing.get(0).getName());
        assertEquals("doc2", forViewing.get(1).getName());
        assertEquals("doc3", forViewing.get(2).getName());
    }

    @Test
    public void testCreatePDFs_validData_pdfCreated() throws Exception {

        DocumentInfoView docView1 = mockDocInfoView("doc1");
        DocumentInfoView docView2 = mockDocInfoView("doc2");
        List<DocumentInfoView> documentInfoViews = Arrays.asList(docView1, docView2);

        PDFDocument pdf1 = mock(PDFDocument.class);
        PDFDocument pdf2 = mock(PDFDocument.class);

        when(docView1.generatePDF()).thenReturn(pdf1);
        when(docView2.generatePDF()).thenReturn(pdf2);

        when(docView1.generatePDF()).thenReturn(pdf1);
        when(docView2.generatePDF()).thenReturn(pdf2);

        List<PDFDocument> results = dataEntryBusiness.createPDFs(documentInfoViews);

        verify(pdf1).setName("doc1");
        verify(pdf2).setName("doc2");

        assertEquals(2, results.size());
    }

    @Test
    public void testWritePDFsToFiles_validData_correctFileNumberReturned() throws Exception {
        List<File> results = writePDFToFiles();
        assertEquals(2, results.size());
    }

    @Test
    public void testWritePDFsToFiles_validData_correctFileNamesReturned() throws Exception {
        List<File> results = writePDFToFiles();
        assertEquals(fileRoot + "pdf1.pdf", results.get(0).getPath());
    }

    private List<File> writePDFToFiles() {
        PDFDocument mockPDF1 = mock(PDFDocument.class);
        PDFDocument mockPDF2 = mock(PDFDocument.class);

        when(mockPDF1.getName()).thenReturn("pdf1");
        when(mockPDF2.getName()).thenReturn("pdf2");

        List<File> results = dataEntryBusiness.writePDFsToFiles(Arrays.asList(mockPDF1, mockPDF2));

        verify(mockPDF1).writeToFile(any(File.class));
        verify(mockPDF2).writeToFile(any(File.class));
        return results;
    }

    @Test
    public void testMakeConcatenatedFile_validParams_returnsCorrectFileName() throws Exception {
        dataEntryBusiness.setPdfConcatenator(mock(PDFConcatenator.class));
        File file = dataEntryBusiness.makeConcatenatedFile(new ArrayList<File>());
        assertEquals(fileRoot + ViewConstants.CONCATENATED_FILE_NAME, file.getPath());
    }

    @Test
    public void testMapAutoMapFields_nullDocs_errorThrown() throws Exception {
        try {
            dataEntryBusiness.mapAutoMapFields(null,
                                               mock(ContactView.class),
                                               mock(ContactView.class));
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_DOCUMENTS_SELECTED, message);
        }

    }

    @Test
    public void testMapAutoMapFields_emptyDocs_errorThrown() throws Exception {
        try {
            dataEntryBusiness.mapAutoMapFields(new ArrayList<DocumentInfoView>(),
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
            dataEntryBusiness.mapAutoMapFields(Arrays.asList(mock(DocumentInfoView.class)),
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
            dataEntryBusiness.mapAutoMapFields(Arrays.asList(mock(DocumentInfoView.class)),
                                               mockCustomer,
                                               null);
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(ViewUtils.NO_BUSINESS_SELECTED, message);
        }
    }

    //MOCKING

    private DocumentInfoView mockDocInfoView(String docName) {
        DocumentInfoView docView = mock(DocumentInfoView.class);
        when(docView.getName()).thenReturn(docName);
        return docView;
    }

}