package com.kuretru.microservices.web.mapper.method;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.mapping.SqlSource;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class UpdateSequenceByIdMethod extends AbstractMethod {

    public UpdateSequenceByIdMethod(String name) {
        super(name);
    }

    @Override
    public MappedStatement injectMappedStatement(Class<?> mapperClass, Class<?> modelClass, TableInfo tableInfo) {
        String sqlTemplate = """
                <script>
                UPDATE %s
                SET sequence = CASE %s
                <foreach collection="list" item="item">
                    WHEN #{item.%s} THEN #{item.sequence}
                </foreach>
                END
                WHERE %s IN
                <foreach collection="list" item="item" open="(" separator="," close=")">
                    #{item.%s}
                </foreach>
                </script>""";
        String tableName = tableInfo.getTableName();
        String keyColumn = tableInfo.getKeyColumn();
        String keyProperty = tableInfo.getKeyProperty();
        String sql = String.format(sqlTemplate, tableName, keyColumn, keyProperty, keyColumn, keyProperty);
        SqlSource sqlSource = languageDriver.createSqlSource(configuration, sql, modelClass);
        return this.addUpdateMappedStatement(mapperClass, modelClass, this.methodName, sqlSource);
    }

}
