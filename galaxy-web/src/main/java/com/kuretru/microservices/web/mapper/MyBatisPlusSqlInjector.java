package com.kuretru.microservices.web.mapper;

import com.baomidou.mybatisplus.core.injector.AbstractMethod;
import com.baomidou.mybatisplus.core.injector.DefaultSqlInjector;
import com.baomidou.mybatisplus.core.metadata.TableInfo;
import com.kuretru.microservices.web.mapper.method.GetMaxSequenceMethod;

import java.util.List;

/**
 * @author 呉真(kuretru) <kuretru@gmail.com>
 */
public class MyBatisPlusSqlInjector extends DefaultSqlInjector {

    @Override
    public List<AbstractMethod> getMethodList(Class<?> mapperClass, TableInfo tableInfo) {
        List<AbstractMethod> methodList = super.getMethodList(mapperClass, tableInfo);
        if (BaseSequenceMapper.class.isAssignableFrom(mapperClass)) {
            methodList.add(new GetMaxSequenceMethod("getMaxSequence"));
            // methodList.add(new UpdateSequenceByUuidsMethod("updateSequenceByUuids"));
        }
        return methodList;
    }

}
