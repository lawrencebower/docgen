package org.lawrencebower.docgen.core.document.component.position;

import org.junit.Test;

import static org.junit.Assert.assertNotNull;

public class DocPositionTest {

    @Test
    public void testGetAlignment_notSet_returnsDefault() throws Exception {
        DocPosition position = new DocPosition(new DocCoordinates(1, 1, 1, 1));
        assertNotNull(position.getHorizontalAlignment());
    }
}
