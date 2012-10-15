<%@ taglib prefix="sf" uri="http://www.springframework.org/tags/form" %>
<%@ include file="/WEB-INF/jsp/include.jsp" %>

hello I am the fields

<jsp:useBean id="sessionData" scope="session" type="org.lawrencebower.docgen.web.model.SessionData"/>

<div>
    <ol>
        <sf:form method="POST"
                 modelAttribute="sessionData"
                 commandName="dataEntry"
                 action="submitFields">

            <c:forEach var="document" items="${sessionData.documents}">

                <br/><c:out value="${document.name}"/><br/><br/>

                <c:forEach var="field" items="${document.editableFields}">
                    <c:if test="${field.editable and field.isTextComponent}">
                        <c:out value="${field.name}"/><br/>
                        <sf:input path="${field.value}"/>
                    </c:if>
                </c:forEach>
            </c:forEach>
            <input name="commit" type="submit"/>
        </sf:form>
    </ol>
</div>