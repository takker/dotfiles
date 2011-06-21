require 'rubygems'
require 'irb/completion'        #単語補完が有効
require 'irb/ext/save-history'  #irbコマンド履歴を残せます。
require 'pp'

# ls command
def ls(arg)
    system "ls #{arg}"
end

# methodがどこで定義されているかを返す
def which(obj, method)
  klass = obj.method(method).owner.to_s
  if klass =~ /\A#<Class:(.+)>\Z/
    klass = $1
    delimiter = "."
  else
    delimiter = "#"
  end
  [klass, method.to_s].join(delimiter)
end

IRB.conf[:AUTO_INDENT]  = true
IRB.conf[:USE_READLINE] = true
IRB.conf[:SAVE_HISTORY] = 1000
IRB.conf[:HISTORY_PATH] = File::expand_path("~/.irb-history")
