(defun c:gg () 

  (initget "eXit Reset Generate File filLet Paibu wireWidth")
  (setq ans (getkword 
              "abc123: [开始生成\(s\)/读取CSV参数文件\(s\)/切换外形框倒圆\ss\)/切换排布方向\(P\)/切换线宽生成\(W\)/恢复默认布线偏好设置\(R\)/退出\(X\)]:<开始生成\(G\)>\n"
            )
  )
)