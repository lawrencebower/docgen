<%@ include file="/WEB-INF/jsp/include.jsp" %>

<jsp:useBean id="products" scope="request"
             type="java.util.List<org.lawrencebower.docgen.web_logic.view.product.ProductView>"/>
<jsp:useBean id="sessionData" scope="session" type="org.lawrencebower.docgen.web.model.SessionData"/>

Select products

<div>
    <br/>
    <c:forEach var="product" items="${products}">

        <s:url var="product_url" value="/productSelect/productId/{productId}">
            <s:param name="productId" value="${product.id}"/>
        </s:url>

        <a href="${product_url}">
            <c:out value="${product.productName}"/>&nbsp;-&nbsp;<c:out value="${product.id}"/>
        </a>
        <br/>
    </c:forEach>
    <br/>
    <br/>
    <c:if test="${sessionData.hasProducts}">
        <table border="1">
            <tr>
                <td>Product Name</td>
                <td>Quantity</td>
            </tr>
            <c:forEach var="selectedProduct" items="${sessionData.selectedProducts}">
                <tr>
                    <td>
                        <c:out value="${selectedProduct.productName}"/>
                        &nbsp;-&nbsp;
                        <c:out value="${selectedProduct.id}"/>
                    </td>
                    <td align="center">
                        <c:out value="${selectedProduct.quantity}"/>
                    </td>
                </tr>
            </c:forEach>
        </table>

        <br/>
        <br/>

        <s:url var="data_entry_url" value="/dataEntry/prepareFields"/>

        <a href="${data_entry_url}">
            <c:out value="Enter data"/>
        </a>

    </c:if>

</div>
