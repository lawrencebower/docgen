package org.lawrencebower.docgen.web_model.view.contact;

import org.junit.Before;
import org.junit.Test;
import org.lawrencebower.docgen.core.exception.DocGenException;

import static junit.framework.TestCase.assertEquals;
import static org.mockito.Mockito.mock;

public class CustomerSelectionTest {

    private CustomerSelection customerSelection;

    @Before
    public void setUp() {
        customerSelection = new CustomerSelection();
    }

    @Test
    public void testSelectCustomer() throws Exception {
        //todo
    }

    @Test
    public void testGetSelectedCustomer() throws Exception {
        //todo
    }

    @Test
    public void testCheckCustomerSet_noCustomer_errorThrown() throws Exception {
        try {
            customerSelection.checkCustomerSet();
        } catch (DocGenException e) {
            String message = e.getMessage();
            assertEquals(CustomerSelection.NO_CUSTOMER_SELECTED, message);
        }
    }

    @Test
    public void testCheckCustomerSet_customerSet_noErrorThrown() throws Exception {
        customerSelection.selectCustomer(mock(ContactView.class));
        customerSelection.checkCustomerSet();
    }
}
