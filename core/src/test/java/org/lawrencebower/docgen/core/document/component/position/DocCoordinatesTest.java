package org.lawrencebower.docgen.core.document.component.position;

import org.junit.Test;

import static junit.framework.Assert.assertEquals;

public class DocCoordinatesTest {

    @Test
    public void testGetXPlusWidth_validArgs_validResult() throws Exception {
        DocCoordinates coordinates = new DocCoordinates(0, 10, 5, 15);
        assertEquals(5, coordinates.getXPlusWidth());
    }

    @Test
    public void testGetYPlusHeight_validArgs_validResult() throws Exception {
        DocCoordinates coordinates = new DocCoordinates(0, 10, 5, 15);
        assertEquals(25, coordinates.getYPlusHeight());
    }
}
